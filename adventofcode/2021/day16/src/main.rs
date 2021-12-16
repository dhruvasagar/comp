use std::io;
use std::str::FromStr;
use std::time::Instant;

fn read_input() -> String {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin
        .lock()
        .lines()
        .map(|x| x.unwrap())
        .collect::<Vec<String>>()[0]
        .chars()
        .map(|l| format!("{:04b}", l.to_digit(16).unwrap()))
        .collect::<Vec<String>>()
        .join("")
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
enum Operator {
    FifteenBitTotalLength { length: i64 },
    ElevenBitPacketCount { count: i64 },
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
enum Packet {
    LiteralValue {
        value: i64,
        version: i64,
        last_index: usize,
    },
    Operator {
        nums: Vec<i64>,
        type_id: i64,
        version: i64,
        versions: Vec<i64>,
        operator: Operator,
        last_index: usize,
        packets: Vec<Packet>,
    },
}

impl Packet {
    fn version_total(&self) -> i64 {
        match self {
            Packet::LiteralValue {
                version,
                value: _,
                last_index: _,
            } => *version,
            Packet::Operator {
                versions,
                nums: _,
                packets: _,
                type_id: _,
                version: _,
                operator: _,
                last_index: _,
            } => versions.iter().sum(),
        }
    }
    fn value(&self) -> i64 {
        match self {
            Packet::LiteralValue {
                value,
                version: _,
                last_index: _,
            } => *value,
            Packet::Operator {
                type_id,
                packets,
                nums: _,
                version: _,
                versions: _,
                operator: _,
                last_index: _,
            } => {
                match type_id {
                    0 => {
                        // sum
                        packets.iter().map(|p| p.value()).sum()
                    }
                    1 => {
                        // product
                        packets.iter().map(|p| p.value()).product()
                    }
                    2 => {
                        // minimum
                        packets.iter().map(|p| p.value()).min().unwrap()
                    }
                    3 => {
                        // maximum
                        packets.iter().map(|p| p.value()).max().unwrap()
                    }
                    5 => {
                        // greater than
                        let p1 = packets[0].clone();
                        let p2 = packets[1].clone();
                        if p1.value() > p2.value() {
                            1
                        } else {
                            0
                        }
                    }
                    6 => {
                        // less than
                        let p1 = packets[0].clone();
                        let p2 = packets[1].clone();
                        if p1.value() < p2.value() {
                            1
                        } else {
                            0
                        }
                    }
                    7 => {
                        // equal to
                        let p1 = packets[0].clone();
                        let p2 = packets[1].clone();
                        if p1.value() == p2.value() {
                            1
                        } else {
                            0
                        }
                    }
                    _ => 0,
                }
            }
        }
    }
}

impl FromStr for Packet {
    type Err = ();
    fn from_str(s: &str) -> Result<Packet, ()> {
        let version = i64::from_str_radix(&s[0..=2], 2).unwrap();
        let type_id = i64::from_str_radix(&s[3..=5], 2).unwrap();
        // println!("s: {}, version: {}, type_id: {}", s, version, type_id);
        match type_id {
            4 => {
                let size = 5;
                let mut start = 6;
                let mut bits: String = String::new();
                while start < s.len() {
                    let end = start + size;
                    if end > s.len() {
                        break;
                    }

                    let sbit = s.chars().nth(start).unwrap();
                    bits += &s[(start + 1)..end].to_string();
                    start += size;
                    if sbit == '0' {
                        break;
                    }
                }
                let value = i64::from_str_radix(bits.as_str(), 2).unwrap();
                Ok(Packet::LiteralValue {
                    value,
                    version,
                    last_index: start,
                })
            }
            _ => {
                // Operator
                match &s[6..7] {
                    "0" => {
                        // parse 15 bits as total length of bits in subpackets
                        let length = i64::from_str_radix(&s[7..22], 2).unwrap();
                        let end = 22 + length as usize;
                        let mut index = 22;
                        let mut nums: Vec<i64> = vec![];
                        let mut vers: Vec<i64> = vec![version];
                        let mut packets: Vec<Packet> = vec![];
                        while index < end {
                            let p = Packet::from_str(&s[index..])?;
                            packets.push(p.clone());
                            match p {
                                Packet::LiteralValue {
                                    value,
                                    version,
                                    last_index,
                                } => {
                                    nums.push(value);
                                    vers.push(version);
                                    index += last_index;
                                }
                                Packet::Operator {
                                    last_index,
                                    versions: vs,
                                    nums: ns,
                                    type_id: _,
                                    version: _,
                                    operator: _,
                                    packets: _,
                                } => {
                                    for n in ns {
                                        nums.push(n);
                                    }
                                    for v in vs {
                                        vers.push(v);
                                    }
                                    index += last_index;
                                }
                            };
                        }

                        Ok(Packet::Operator {
                            nums,
                            type_id,
                            version,
                            packets,
                            versions: vers,
                            last_index: end,
                            operator: Operator::FifteenBitTotalLength { length },
                        })
                    }
                    "1" => {
                        // parse 11 bits as number of packets
                        let count = i64::from_str_radix(&s[7..18], 2).unwrap();
                        let mut index = 18;
                        let mut nums: Vec<i64> = vec![];
                        let mut vers: Vec<i64> = vec![version];
                        let mut packets: Vec<Packet> = vec![];
                        let mut pcount = 0;
                        while pcount < count as usize {
                            let p = Packet::from_str(&s[index..])?;
                            packets.push(p.clone());
                            match p {
                                Packet::LiteralValue {
                                    value,
                                    version,
                                    last_index,
                                } => {
                                    nums.push(value);
                                    vers.push(version);
                                    index += last_index;
                                }
                                Packet::Operator {
                                    last_index,
                                    versions: vs,
                                    nums: ns,
                                    version: _,
                                    operator: _,
                                    packets: _,
                                    type_id: _,
                                } => {
                                    for n in ns {
                                        nums.push(n);
                                    }
                                    for v in vs {
                                        vers.push(v);
                                    }
                                    index += last_index;
                                }
                            }
                            pcount += 1;
                        }

                        Ok(Packet::Operator {
                            nums,
                            version,
                            packets,
                            type_id,
                            versions: vers,
                            last_index: index,
                            operator: Operator::ElevenBitPacketCount { count },
                        })
                    }
                    _ => Err(()),
                }
            }
        }
    }
}

fn part1(packet: Packet) -> String {
    format!("{}", packet.version_total())
}

fn part2(packet: Packet) -> String {
    format!("{}", packet.value())
}

fn main() -> Result<(), ()> {
    let s = Instant::now();
    let bits = read_input();
    let packet = Packet::from_str(bits.as_str())?;

    let s1 = Instant::now();
    println!("{}", part1(packet.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(packet.clone()));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!(
        "Time for part1: {:?}, part2: {:?}, total with IO: {:?}",
        e1, e2, e
    );
    Ok(())
}
