use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use std::io::{self};

use super::error::{NativeTypeError, OpcodeError};
use crate::runtime::{
    native_type::{self, NativeType},
    opcode::{self, Opcode},
};

pub fn read_native_type<R>(cursor: &mut R) -> Result<NativeType, NativeTypeError>
where
    R: io::Read,
{
    let byte = cursor.read_u8()?;
    match byte {
        native_type::unit => Ok(NativeType::unit),
        native_type::bool => Ok(NativeType::bool),
        native_type::u8 => Ok(NativeType::u8),
        native_type::u16 => Ok(NativeType::u16),
        native_type::u32 => Ok(NativeType::u32),
        native_type::u64 => Ok(NativeType::u64),
        native_type::i8 => Ok(NativeType::i8),
        native_type::i16 => Ok(NativeType::i16),
        native_type::i32 => Ok(NativeType::i32),
        native_type::i64 => Ok(NativeType::i64),
        native_type::f32 => Ok(NativeType::f32),
        native_type::f64 => Ok(NativeType::f64),
        _ => Err(NativeTypeError::IllegalNativeType),
    }
}

pub fn write_native_type<W>(cursor: &mut W, native_type: &NativeType) -> io::Result<()>
where
    W: io::Write,
{
    cursor.write_u8((*native_type).into())
}

pub fn read_opcode<R>(cursor: &mut R) -> Result<Opcode, OpcodeError>
where
    R: io::Read,
{
    let byte = cursor.read_u8()?;
    match byte {
        opcode::nop => Ok(Opcode::nop),

        opcode::pop => Ok(Opcode::pop),
        opcode::pop_n => Ok(Opcode::pop_n(cursor.read_u8()?)),

        opcode::dup => Ok(Opcode::dup),
        opcode::dup_n => Ok(Opcode::dup_n(cursor.read_u8()?)),

        opcode::dbg => Ok(Opcode::dbg(read_native_type(cursor)?)),

        opcode::jmp => Ok(Opcode::jmp(cursor.read_u32::<LE>()?)),
        opcode::jmp_if => Ok(Opcode::jmp_if(cursor.read_u32::<LE>()?)),
        opcode::ret => Ok(Opcode::ret),
        opcode::call => Ok(Opcode::call(cursor.read_u32::<LE>()?)),
        opcode::call_addr => Ok(Opcode::call_addr),

        opcode::ld_loc => Ok(Opcode::ld_loc(cursor.read_u8()?)),
        opcode::ld_loc_n => Ok(Opcode::ld_loc_n(cursor.read_u8()?, cursor.read_u8()?)),
        opcode::st_loc => Ok(Opcode::st_loc(cursor.read_u8()?)),
        opcode::st_loc_n => Ok(Opcode::st_loc_n(cursor.read_u8()?, cursor.read_u8()?)),

        opcode::alloc => Ok(Opcode::alloc),
        opcode::alloc_n => Ok(Opcode::alloc_n(cursor.read_u8()?)),
        opcode::ld_heap => Ok(Opcode::ld_heap),
        opcode::ld_heap_n => Ok(Opcode::ld_heap_n(cursor.read_u8()?)),
        opcode::st_heap => Ok(Opcode::st_heap),
        opcode::st_heap_n => Ok(Opcode::st_heap_n(cursor.read_u8()?)),
        opcode::realloc_loc => Ok(Opcode::realloc_loc(cursor.read_u8()?)),
        opcode::realloc_loc_n => Ok(Opcode::realloc_loc_n(cursor.read_u8()?, cursor.read_u8()?)),

        opcode::entry_point => Ok(Opcode::entry_point(cursor.read_u32::<LE>()?)),
        opcode::function => {
            let n = cursor.read_u16::<LE>()?;
            let name = {
                let mut buf = vec![0; n as usize];
                cursor.read_exact(&mut buf)?;
                String::from_utf8(buf)?
            };

            let param_count = cursor.read_u8()?;
            let local_count = cursor.read_u8()?;

            Ok(Opcode::function(name, param_count, local_count))
        }

        opcode::ld_u8 => Ok(Opcode::ld_u8(cursor.read_u8()?)),
        opcode::ld_u16 => Ok(Opcode::ld_u16(cursor.read_u16::<LE>()?)),
        opcode::ld_u32 => Ok(Opcode::ld_u32(cursor.read_u32::<LE>()?)),
        opcode::ld_u64 => Ok(Opcode::ld_u64(cursor.read_u64::<LE>()?)),
        opcode::ld_i8 => Ok(Opcode::ld_i8(cursor.read_i8()?)),
        opcode::ld_i16 => Ok(Opcode::ld_i16(cursor.read_i16::<LE>()?)),
        opcode::ld_i32 => Ok(Opcode::ld_i32(cursor.read_i32::<LE>()?)),
        opcode::ld_i64 => Ok(Opcode::ld_i64(cursor.read_i64::<LE>()?)),
        opcode::ld_f32 => Ok(Opcode::ld_f32(cursor.read_f32::<LE>()?)),
        opcode::ld_f64 => Ok(Opcode::ld_f64(cursor.read_f64::<LE>()?)),
        opcode::ld_unit => Ok(Opcode::ld_unit),

        opcode::add => Ok(Opcode::add(read_native_type(cursor)?)),
        opcode::sub => Ok(Opcode::sub(read_native_type(cursor)?)),
        opcode::mul => Ok(Opcode::mul(read_native_type(cursor)?)),
        opcode::div => Ok(Opcode::div(read_native_type(cursor)?)),
        opcode::rem => Ok(Opcode::rem(read_native_type(cursor)?)),
        opcode::eq => Ok(Opcode::eq(read_native_type(cursor)?)),
        opcode::ne => Ok(Opcode::ne(read_native_type(cursor)?)),
        opcode::le => Ok(Opcode::le(read_native_type(cursor)?)),
        opcode::lt => Ok(Opcode::lt(read_native_type(cursor)?)),
        opcode::ge => Ok(Opcode::ge(read_native_type(cursor)?)),
        opcode::gt => Ok(Opcode::gt(read_native_type(cursor)?)),
        opcode::bitand => Ok(Opcode::bitand(read_native_type(cursor)?)),
        opcode::bitor => Ok(Opcode::bitor(read_native_type(cursor)?)),
        opcode::bitxor => Ok(Opcode::bitxor(read_native_type(cursor)?)),

        opcode::neg => Ok(Opcode::neg(read_native_type(cursor)?)),

        opcode::todo => Ok(Opcode::todo),
        opcode::unreachable => Ok(Opcode::unreachable),

        _ => Err(OpcodeError::IllegalOpcode),
    }
}

pub fn write_opcode<W>(cursor: &mut W, opcode: &Opcode) -> io::Result<()>
where
    W: io::Write,
{
    match opcode {
        Opcode::nop => cursor.write_u8(opcode::nop),
        Opcode::pop => cursor.write_u8(opcode::pop),
        Opcode::pop_n(n) => {
            cursor.write_u8(opcode::pop_n)?;
            cursor.write_u8(*n)?;
            Ok(())
        }
        Opcode::dup => cursor.write_u8(opcode::dup),
        Opcode::dup_n(n) => {
            cursor.write_u8(opcode::dup_n)?;
            cursor.write_u8(*n)?;
            Ok(())
        }
        Opcode::dbg(ty) => {
            cursor.write_u8(opcode::dbg)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }

        Opcode::jmp(addr) => {
            cursor.write_u8(opcode::jmp)?;
            cursor.write_u32::<LE>(*addr)?;
            Ok(())
        }
        Opcode::jmp_if(addr) => {
            cursor.write_u8(opcode::jmp_if)?;
            cursor.write_u32::<LE>(*addr)?;
            Ok(())
        }
        Opcode::ret => cursor.write_u8(opcode::ret),
        Opcode::call(addr) => {
            cursor.write_u8(opcode::call)?;
            cursor.write_u32::<LE>(*addr)?;
            Ok(())
        }
        Opcode::call_addr => cursor.write_u8(opcode::call_addr),

        Opcode::ld_loc(loc) => {
            cursor.write_u8(opcode::ld_loc)?;
            cursor.write_u8(*loc)?;
            Ok(())
        }
        Opcode::ld_loc_n(loc, n) => {
            cursor.write_u8(opcode::ld_loc_n)?;
            cursor.write_u8(*loc)?;
            cursor.write_u8(*n)?;
            Ok(())
        }
        Opcode::st_loc(loc) => {
            cursor.write_u8(opcode::st_loc)?;
            cursor.write_u8(*loc)?;
            Ok(())
        }
        Opcode::st_loc_n(loc, n) => {
            cursor.write_u8(opcode::st_loc_n)?;
            cursor.write_u8(*loc)?;
            cursor.write_u8(*n)?;
            Ok(())
        }

        Opcode::alloc => cursor.write_u8(opcode::alloc),
        Opcode::alloc_n(n) => {
            cursor.write_u8(opcode::alloc_n)?;
            cursor.write_u8(*n)?;
            Ok(())
        }

        Opcode::ld_heap => cursor.write_u8(opcode::ld_heap),
        Opcode::ld_heap_n(n) => {
            cursor.write_u8(opcode::ld_heap_n)?;
            cursor.write_u8(*n)?;
            Ok(())
        }

        Opcode::st_heap => cursor.write_u8(opcode::st_heap),
        Opcode::st_heap_n(n) => {
            cursor.write_u8(opcode::st_heap_n)?;
            cursor.write_u8(*n)?;
            Ok(())
        }

        Opcode::realloc_loc(loc) => {
            cursor.write_u8(opcode::realloc_loc)?;
            cursor.write_u8(*loc)?;
            Ok(())
        }
        Opcode::realloc_loc_n(loc, n) => {
            cursor.write_u8(opcode::realloc_loc_n)?;
            cursor.write_u8(*loc)?;
            cursor.write_u8(*n)?;
            Ok(())
        }

        Opcode::entry_point(addr) => {
            cursor.write_u8(opcode::entry_point)?;
            cursor.write_u32::<LE>(*addr)?;
            Ok(())
        }
        Opcode::function(name, param_count, local_count) => {
            let len = name.len() as u16;
            let bytes = name.as_bytes();

            cursor.write_u8(opcode::function)?;
            cursor.write_u16::<LE>(len)?;
            cursor.write_all(bytes)?;
            cursor.write_u8(*param_count)?;
            cursor.write_u8(*local_count)?;

            Ok(())
        }

        Opcode::ld_u8(num) => {
            cursor.write_u8(opcode::ld_u8)?;
            cursor.write_u8(*num)?;
            Ok(())
        }
        Opcode::ld_u16(num) => {
            cursor.write_u8(opcode::ld_u16)?;
            cursor.write_u16::<LE>(*num)?;
            Ok(())
        }
        Opcode::ld_u32(num) => {
            cursor.write_u8(opcode::ld_u32)?;
            cursor.write_u32::<LE>(*num)?;
            Ok(())
        }
        Opcode::ld_u64(num) => {
            cursor.write_u8(opcode::ld_u64)?;
            cursor.write_u64::<LE>(*num)?;
            Ok(())
        }
        Opcode::ld_i8(num) => {
            cursor.write_u8(opcode::ld_i8)?;
            cursor.write_i8(*num)?;
            Ok(())
        }
        Opcode::ld_i16(num) => {
            cursor.write_u8(opcode::ld_i16)?;
            cursor.write_i16::<LE>(*num)?;
            Ok(())
        }
        Opcode::ld_i32(num) => {
            cursor.write_u8(opcode::ld_i32)?;
            cursor.write_i32::<LE>(*num)?;
            Ok(())
        }
        Opcode::ld_i64(num) => {
            cursor.write_u8(opcode::ld_i64)?;
            cursor.write_i64::<LE>(*num)?;
            Ok(())
        }
        Opcode::ld_f32(num) => {
            cursor.write_u8(opcode::ld_f32)?;
            cursor.write_f32::<LE>(*num)?;
            Ok(())
        }
        Opcode::ld_f64(num) => {
            cursor.write_u8(opcode::ld_f64)?;
            cursor.write_f64::<LE>(*num)?;
            Ok(())
        }
        Opcode::ld_unit => cursor.write_u8(opcode::ld_unit),

        Opcode::add(ty) => {
            cursor.write_u8(opcode::add)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }
        Opcode::sub(ty) => {
            cursor.write_u8(opcode::sub)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }
        Opcode::mul(ty) => {
            cursor.write_u8(opcode::mul)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }
        Opcode::div(ty) => {
            cursor.write_u8(opcode::div)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }
        Opcode::rem(ty) => {
            cursor.write_u8(opcode::rem)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }
        Opcode::eq(ty) => {
            cursor.write_u8(opcode::eq)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }
        Opcode::ne(ty) => {
            cursor.write_u8(opcode::ne)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }
        Opcode::le(ty) => {
            cursor.write_u8(opcode::le)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }
        Opcode::lt(ty) => {
            cursor.write_u8(opcode::lt)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }
        Opcode::ge(ty) => {
            cursor.write_u8(opcode::ge)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }
        Opcode::gt(ty) => {
            cursor.write_u8(opcode::gt)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }
        Opcode::bitand(ty) => {
            cursor.write_u8(opcode::bitand)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }
        Opcode::bitor(ty) => {
            cursor.write_u8(opcode::bitor)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }
        Opcode::bitxor(ty) => {
            cursor.write_u8(opcode::bitxor)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }

        Opcode::neg(ty) => {
            cursor.write_u8(opcode::neg)?;
            write_native_type(cursor, ty)?;
            Ok(())
        }

        Opcode::todo => cursor.write_u8(opcode::todo),
        Opcode::unreachable => cursor.write_u8(opcode::unreachable),
    }
}
