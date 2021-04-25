mod cpu;
use std::fs;
use std::env;
use std::thread;
use std::time::Duration;
fn main() {
    let res = run();
    if res.is_err(){
        println!("{}", res.unwrap_err());
    }
}
fn run() -> Result<(), String> {
    let mut DEBUG = false;
    let args: Vec<String> = env::args().collect();
    if args.len() == 3 {
        if args[2] == "dbg" || args[2] == "DBG"{

            DEBUG = true;
        } else {
            return Err("Error argument 2 is not DBG or dbg".to_owned());
        }
    }
    let machine_code = fs::read(args[1].as_str());
    if machine_code.is_err() {
        return Err(format!("Error reading file: {}", machine_code.unwrap_err()));
    }
    let machine_code = machine_code.unwrap();
    if machine_code.len() >= 65536{
        return Err("binary file outside of addressing range".to_owned());
    }
    let mut emulator = cpu::new(machine_code.as_slice(), machine_code.len());
    let mut res = Ok(());
    let time_between_clock_cycles = Duration::from_micros(1);
    let mut total_clocks = 0;
    let mut IRQ = false;
    let mut NMI = false;
    emulator.interrupt(0, 0xFFFC);
    loop {
        res = emulator.clock(DEBUG);
        if !res.is_ok() {
            println!("{}", res.unwrap_err());
            break;}
        total_clocks += 1;
        thread::sleep(time_between_clock_cycles);
        if emulator.clocks == 0 {
            if emulator.Flags & cpu::IFLAG != 0 && IRQ == true{
                emulator.interrupt(0, 0xFFFE);
                IRQ = false;
            } else if NMI == true{
                emulator.interrupt(0, 0xFFFA);
                NMI = false;
            }
        }
    }

    emulator.print_state();

    println!("mem: 10 {:#04x}", emulator.Mem[10]);
    println!("mem: FFFE {:#06x}", u16::from_le_bytes([emulator.Mem[0xFFFE], emulator.Mem[0xFFFF]]));

    let var = 0x47;
    println!("HEX: {:#04x}, DECIMAL: {}", var, cpu::dec_to_hex(var));
    println!("DECIMAL: {}, HEX: {:#04x}", cpu::dec_to_hex(var), cpu::hex_to_dec(11));
    println!("Total Clock Cycles: {}", total_clocks);
    Ok(())
}
