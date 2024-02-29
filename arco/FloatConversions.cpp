#include "FloatConversions.h"

#include <cmath>
#include <iostream>

#include "Source.h"

namespace arco {
namespace FD {

    // Double data
    // 

    constexpr ulen MAX_DOUBLE_DECIMAL_DIGITS = 16;
    constexpr u64 POW10_INT64_TABLE[] = {
        1,
        10,
        100,
        1000,
        10000,
        100000,
        1000000,
        10000000,
        100000000,
        1000000000,
        10000000000,
        100000000000,
        1000000000000,
        10000000000000,
        100000000000000,
        1000000000000000,
        10000000000000000,
        100000000000000000,
        1000000000000000000,
        10000000000000000000,
    };

    constexpr i64 DOUBLE_POW10_TABLE_LIMIT = 22;
    constexpr double POW10_DOUBLE_TABLE[DOUBLE_POW10_TABLE_LIMIT+1] = {
        1.0E0 , 1.0E1 , 1.0E2 , 1.0E3 , 1.0E4 , 1.0E5,
        1.0E6 , 1.0E7 , 1.0E8 , 1.0E9 , 1.0E10, 1.0E11,
        1.0E12, 1.0E13, 1.0E14, 1.0E15, 1.0E16, 1.0E17,
        1.0E18, 1.0E19, 1.0E20, 1.0E21, 1.0E22,
    };

    constexpr double POW10_DOUBLE_BIG_TABLE[] = {
        1.0E16, 1.0E32, 1.0E64, 1.0E128, 1.0E256
    };

    constexpr double POW10_DOUBLE_TINY_TABLE[] = {
        1E-16, 1E-32, 1E-64, 1E-128, 1E-256
    };

    // Single Data
    //

    constexpr ulen MAX_SINGLE_DECIMAL_DIGITS = 8;

    constexpr i64 SINGLE_POW10_TABLE_LIMIT = 10;
    constexpr float POW10_SINGLE_TABLE[SINGLE_POW10_TABLE_LIMIT+1] = {
        1.0E0f, 1.0E1f, 1.0E2f, 1.0E3f, 1.0E4f , 1.0E5f,
        1.0E6f, 1.0E7f, 1.0E8f, 1.0E9f, 1.0E10f,
    };

    // Big Integer data

    constexpr ulen POW5_TABLE_SIZE = 13;
    constexpr u32 POW5_TABLE[POW5_TABLE_SIZE] = {
        1,
        5,
        5*5,
        5*5*5,
        5*5*5*5,
        5*5*5*5*5,
        5*5*5*5*5*5,
        5*5*5*5*5*5*5,
        5*5*5*5*5*5*5*5,
        5*5*5*5*5*5*5*5*5,
        5*5*5*5*5*5*5*5*5*5,
        5*5*5*5*5*5*5*5*5*5*5,
        5*5*5*5*5*5*5*5*5*5*5*5,
    };

    constexpr u64 U64_MASK = 0xffffffffull;

    constexpr ulen MAX_POW5_CACHE = 100;
    static BigIntFD POW5_CACHE[MAX_POW5_CACHE];
    
#define MAXX(a, b) (((a) > (b)) ? (a) : (b))
#define MINN(a, b) (((a) > (b)) ? (b) : (a))

    BigIntFD GetBigIntFromPow5(i64 P5) {
        if (P5 < MAX_POW5_CACHE) {
            return POW5_CACHE[P5];
        }

        // 5^pow5 = 5^(q+r) = 5^q * 5^r
        // divide the power in 2.
        // q = pow5/2, r = pow5 - q
        i64 q = P5 >> 1;
        i64 r = P5 - q;

        BigIntFD bq = GetBigIntFromPow5(q);
        if (r < POW5_TABLE_SIZE) {
            return bq.Multiply(POW5_TABLE[r]);
        } else {
            BigIntFD br = GetBigIntFromPow5(r);
            return bq.Multiply(br);
        }
    }

    BigIntFD::~BigIntFD() {
        delete Blocks;
        Blocks = nullptr;
    }

    BigIntFD::BigIntFD(const BigIntFD& o) {
        this->Length = o.Length;
        this->Blocks = new u32[o.Length];
        memcpy(Blocks, o.Blocks, o.Length * sizeof(u32));
    }

    BigIntFD::BigIntFD(BigIntFD&& o) noexcept {
        this->Length = std::exchange(o.Length, 0);
        this->Blocks = std::exchange(o.Blocks, nullptr);
    }

    BigIntFD::BigIntFD(u32 B1, u32 B2, ulen Offset) {
        if (B1 == 0 && B2 == 0) {
            // Shifting 0 by Offset is still just 0.
            Blocks = nullptr;
            Length = 0;
            return;
        } 
    
        Blocks = new u32[Offset + 2];
        memset(Blocks, 0, Offset * sizeof(u32));
        Blocks[Offset]   = B1;
        Blocks[Offset+1] = B2;
        Length = Offset + 2;
        if (B2 == 0) {
            // Last block is zero, trim.
            --Length;
        }
    }

    BigIntFD::BigIntFD(u32 B1, u32 B2, u32 B3, ulen Offset) {
        Blocks = new u32[Offset + 3];
        memset(Blocks, 0, Offset * sizeof(u32));
        Blocks[Offset]   = B1;
        Blocks[Offset+1] = B2;
        Blocks[Offset+2] = B3;
        Length = Offset + 3;
        TrimLeadingZeros();
    }

    BigIntFD& BigIntFD::operator=(const BigIntFD& o) {
        this->Length = o.Length;
        this->Blocks = new u32[o.Length];
        memcpy(Blocks, o.Blocks, o.Length * sizeof(u32));
        return *this;
    }

    BigIntFD& BigIntFD::operator=(BigIntFD&& o) noexcept {
        this->~BigIntFD(); // still need to cleanup in case we allocated for this object already.
        this->Length = std::exchange(o.Length, 0);
        this->Blocks = std::exchange(o.Blocks, nullptr);
        return *this;
    }

    void BigIntFD::MultiplyAndAdd(u32 Mult, u32 Add) {
        const u64 V = Mult & U64_MASK;
        u32* Ptr = Blocks;
        u32* End = Blocks + Length;
        u64 Product = V * (*Ptr & U64_MASK) + (Add & U64_MASK);
        *Ptr = static_cast<u32>(Product);
        Product >>= 32; // Get carry
        ++Ptr;
        while (Ptr != End) {
            Product += V * (*Ptr & U64_MASK);
            *Ptr = static_cast<u32>(Product);
            Product >>= 32; // Get carry
            ++Ptr;
        }
        if (Product != 0) { // Still left over carry.
            Blocks[Length++] = static_cast<u32>(Product);
        }
    }

    BigIntFD BigIntFD::Multiply(BigIntFD& o) {
        if (this->Length == 0) return BigIntFD{}; // Return zero
        if (o.Length == 0)     return BigIntFD{}; // Return zero
        if (this->Length == 1)
            return o.Multiply(Blocks[0]);
        if (o.Length == 1)
            return this->Multiply(o.Blocks[0]);

        BigIntFD Res;
        // Need to clear the result data because it is being used in
        // the multiplication loop and it initialially has random data.
        Res.Length = this->Length + o.Length;
        Res.Blocks = new u32[Res.Length];
        memset(Res.Blocks, 0, Res.Length * sizeof(u32));

        u32* OutPtr = this->Blocks;
        u32* OutEnd = this->Blocks + Length;
        u32* ResPtr = Res.Blocks;
        while (OutPtr != OutEnd) {
            u64 Multiplier = *OutPtr & U64_MASK;
            u64 Product = 0;
            u32* InrPtr = o.Blocks;
            u32* InrEnd = o.Blocks + o.Length;
            u32* ResInrPtr = ResPtr;
            while (InrPtr != InrEnd) {
                Product += (*ResInrPtr & U64_MASK) + Multiplier * (*InrPtr & U64_MASK);
                *ResInrPtr = static_cast<u32>(Product);
                Product >>= 32; // Get carry.
                ++InrPtr;
                ++ResInrPtr;
            }
            *ResInrPtr = static_cast<u32>(Product);

            ++OutPtr;
            ++ResPtr;
        }

        if (Res.Blocks[Res.Length - 1] == 0) {
            --Res.Length;
        }

        return Res;
    }

    BigIntFD BigIntFD::Multiply(u32 Constant) {
        if (this->Length == 0) return BigIntFD{}; // Return zero

        BigIntFD Res;
        Res.Blocks = new u32[this->Length + 1];

        u64 Value = Constant & U64_MASK;
        u32* Ptr = Blocks;
        u32* End = Blocks + Length;
        u32* ResPtr = Res.Blocks; 
        u64 Carry = 0;
        while (Ptr != End) {
            u64 Product = (*Ptr & U64_MASK) * Value + Carry;
            *ResPtr = static_cast<u32>(Product);
            Carry = Product >> 32;
            ++Ptr;
            ++ResPtr;
        }

        if (Carry != 0) {
            *ResPtr = static_cast<u32>(Carry);
            Res.Length = this->Length + 1;
        } else {
            Res.Length = this->Length;
        }
        return Res;
    }

    BigIntFD BigIntFD::Multiply(u32 Constant1, u32 Constant2) {
        BigIntFD Res;
        Res.Blocks = new u32[this->Length + 2];

        u64 Value = Constant1 & U64_MASK;
        u32* Ptr = Blocks;
        u32* End = Blocks + Length;
        u32* ResPtr = Res.Blocks; 
        u64 Carry = 0;
        while (Ptr != End) {
            u64 Product = Value * (*Ptr & U64_MASK) + Carry;
            *ResPtr = static_cast<u32>(Product);
            Carry = Product >> 32;
            ++Ptr;
            ++ResPtr;
        }
        *ResPtr = static_cast<u32>(Carry);
        Value = Constant2 & U64_MASK;
        Carry = 0;
        Ptr = Blocks;
        ResPtr = Res.Blocks + 1;
        while (Ptr != End) {
            u64 Product = (*ResPtr & U64_MASK) + Value * (*Ptr & U64_MASK) + Carry;
            *ResPtr = static_cast<u32>(Product);
            Carry = Product >> 32;
            ++Ptr;
            ++ResPtr;
        }
    
        if (Carry != 0) {
            *ResPtr = static_cast<u32>(Carry);
            Res.Length = this->Length + 2;
        } else {
            Res.Length = this->Length + 1;
        }

        return Res;
    }

    BigIntFD BigIntFD::MultiplyPow5(i64 P5) {
        if (P5 < POW5_TABLE_SIZE) {
            return Multiply(POW5_TABLE[P5]);
        } else {
            BigIntFD BigPow5 = GetBigIntFromPow5(P5);
            return this->Multiply(BigPow5);
        }
    }

    BigIntFD BigIntFD::LeftShift(i64 Shift) {
        // TODO: if offsets are ever included it may be
        // possible to improve performance here by
        // instead of creating new allocation just reusing
        // the old data and setting the offset to the block
        // shift.

        if (Shift == 0 || this->Length == 0) {
            return *this;
        }

        i64 SftBlockIdx = Shift >> 5;
        u32 SftModIdx   = Shift & 31;
        BigIntFD Res;

        if (SftModIdx == 0) {
            // Block aligned.
            Res.Length = this->Length + SftBlockIdx;
            Res.Blocks = new u32[Res.Length];

            // Zero low blocks.
            memset(Res.Blocks, 0, SftBlockIdx * sizeof(u32));
            // Copy blocks into higher indexes
            memcpy(Res.Blocks + SftBlockIdx,
                   this->Blocks,
                   this->Length * sizeof(u32));
        } else {
            // Slide the block we are working with by the shift
            // then append the high bits of the previous block.

            Res.Blocks = new u32[Length + SftBlockIdx + 1];

            u32* FmPtr = Blocks + Length - 1;
            u32* ToPtr = Res.Blocks + Length + SftBlockIdx;

            // How far to shift to extract the previous block's high bits which become the block we
            // are working withs low bits.
            u32 LowBitsShift = 32 - SftModIdx;

            u32 HighBits = 0;
            u32 LowBits = *FmPtr >> LowBitsShift;
            while (FmPtr > Blocks) {
                *ToPtr = HighBits | LowBits;
                HighBits = *FmPtr << SftModIdx;

                --FmPtr;
                --ToPtr;
            
                LowBits = *FmPtr >> LowBitsShift;
            }

            // final block which is also the lowest block only
            // has high bits because its lower bits were shifted.
            *ToPtr = HighBits | LowBits;
            *(ToPtr-1) = *FmPtr << SftModIdx;

            // Zero the low blocks.
            memset(Res.Blocks, 0, SftBlockIdx * sizeof(u32));

            u32 ResLength = Length + SftBlockIdx + 1;
            if (Res.Blocks[ResLength - 1] == 0) {
                --ResLength;
            }

            Res.Length = ResLength;
        }
        return Res;
    }

    
    BigIntFD BigIntFD::Subtract(BigIntFD& Sub) {
    
        BigIntFD Res;
        Res.Length = this->Length;
        Res.Blocks = new u32[Res.Length];

        // TODO: could improve with pointer arithmetic
        i64 Barrow = 0;
        ulen Idx = 0;
        for (; Idx < Sub.Length; Idx++) {	
            i64 Difference = static_cast<i64>(this->Blocks[Idx] & U64_MASK) -
                             static_cast<i64>(Sub.Blocks[Idx]) + Barrow;
            Res.Blocks[Idx] = static_cast<u32>(Difference);
            Barrow = Difference >> 32;
        }
        for (; Barrow != 0 && Idx < this->Length; Idx++) {
            u64 Carry = (this->Blocks[Idx] & U64_MASK) + Barrow;
            Res.Blocks[Idx] = static_cast<u32>(Carry);
            Barrow = Carry >> 32;
        }
    
        Res.TrimLeadingZeros();

        return Res;
    }

    void BigIntFD::TrimLeadingZeros() {
        u32* Ptr = Blocks + Length - 1;
        while (Ptr >= Blocks) {
            if (*Ptr == 0) --Length;
            else break;
            --Ptr;
        }
    }

    void BigIntFD::PrintBlocks() {
        for (int i = 0; i < Length; i++) {
            std::cout << Blocks[i] << "L, ";
        }
    }

    
    i64 BigIntFD::Compare(BigIntFD& o) {
        if (this->Length > o.Length) {
            return +1;
        } else if (this->Length < o.Length) {
            return -1;
        }
    
        // Traverse in reverse order because want to compare
        // highest significant blocks first.
        for (i64 i = this->Length - 1; i >= 0; i--) {
            u32 LHSV = this->Blocks[i];
            u32 RHSV = o.Blocks[i];
            if (LHSV != RHSV) {
                return LHSV < RHSV ? -1 : +1;
            }
        }

        // Exact match.
        return 0;
    }

    
    ulen GetNumberOfLeadingZeros(u32 Int) {
        if (Int == 0) return 32;
        ulen N = 31;
        if (Int >= 1<<16) { N -= 16; Int >>= 16; }
        if (Int >= 1<<8 ) { N -=  8; Int >>= 8;  }
        if (Int >= 1<<4 ) { N -=  4; Int >>= 4;  }
        if (Int >= 1<<2 ) { N -=  2; Int >>= 2;  }
        return N - (Int >> 1);
    }

    ulen GetNumberOfLeadingZeros(u64 Int) {
        u32 HighBits = static_cast<u32>(Int >> 32ull);
        return HighBits == 0 ? 32 + GetNumberOfLeadingZeros(static_cast<u32>(Int))
                             : GetNumberOfLeadingZeros(HighBits);
    }

    ulen GetNumberOfTrailingZeros(u32 Int) {
        Int = ~Int & (Int - 1);
        if (Int == 0) return 0;
        ulen N = 1;
        if (Int > 1<<16) { N += 16; Int >>= 16; }
        if (Int > 1<< 8) { N +=  8; Int >>=  8; }
        if (Int > 1<< 4) { N +=  4; Int >>=  4; }
        if (Int > 1<< 2) { N +=  2; Int >>=  2; }
        return N + (Int >> 1);
    }

    ulen GetNumberOfTrailingZeros(u64 Int) {
        u32 LowBits = static_cast<u32>(Int);
        return LowBits == 0 ? 32 + GetNumberOfTrailingZeros(static_cast<u32>(Int >> 32))
                            : GetNumberOfTrailingZeros(static_cast<u32>(LowBits));
    }

    BigIntFD CreateBigIntFromDecimalDigits(
        u64 IValue, char* Digits, ulen DigitCount, ulen NDigits
        ) {
    
        BigIntFD Res;
        Res.Blocks    = new u32[MAXX((NDigits+8)/9, 2)];
        Res.Blocks[0] = static_cast<u32>(IValue);
        Res.Blocks[1] = static_cast<u32>(IValue >> 32ull);
        Res.Length = 2;

        // multiplying by 10^5 to shift over then adding
        // the decimal digits.
        ulen Idx = 0;
        while (Idx + 5 < DigitCount) {

            u32 Value = static_cast<u32>(Digits[Idx++]) - '0';
            for (ulen i = 1; i < 5; i++) {
                Value *= 10;
                Value += static_cast<u32>(Digits[Idx++]) - '0';
            }
            Res.MultiplyAndAdd(100000, Value); // shift 10^5 then add into empty zeros
        }
        // Same process except there isn't 10^5 left to shift so
        // shifting by 10^n (n < 5) and adding the digits.
        u32 Shift = 1;
        u32 Value = 0;
        while (Idx < DigitCount) {
            Value *= 10;
            Value += static_cast<u32>(Digits[Idx++]) - '0';
            Shift *= 10;
        }
        if (Shift != 1) {
            Res.MultiplyAndAdd(Shift, Value);
        }

        Res.TrimLeadingZeros();
        return Res;
    }

    
    BigIntFD GetBigIntFromSigfPow2Pow5(u64 Sigf, i64 P5, i64 P2) {
        u32 B1 = static_cast<u32>(Sigf);
        u32 B2 = static_cast<u32>(Sigf >> 32ull);
        i64 SftBlockIdx = P2 >> 5;
        u32 SftModIdx   = P2 & 31;

        if (P5 != 0) {
            if (P5 < POW5_TABLE_SIZE) {
                u64 Pow5 = POW5_TABLE[P5] & U64_MASK;
                u64 Product = Pow5 * (B1 & U64_MASK);
                B1 = static_cast<u32>(Product);
                u64 Carry = Product >> 32;
                Product = Pow5 * (B2 & U64_MASK) + Carry;
                B2 = static_cast<u32>(Product);
                u32 B3 = static_cast<u32>(Product >> 32);
                if (SftModIdx == 0) {
                    // Shift is block aligned so can optimize by
                    // just moving the values over the correct number
                    // of blocks.
                    return BigIntFD(B1, B2, B3, SftBlockIdx);
                } else {
                    BigIntFD Res;
                    Res.Blocks = new u32[SftBlockIdx + 4];
                    Res.Blocks[SftBlockIdx+0] = B1 << SftModIdx;
                    Res.Blocks[SftBlockIdx+1] = (B2 << SftModIdx) | (B1 >> (32 - SftModIdx));
                    Res.Blocks[SftBlockIdx+2] = (B3 << SftModIdx) | (B2 >> (32 - SftModIdx));
                    Res.Blocks[SftBlockIdx+3] = B3 >> (32 - SftModIdx);
                    Res.Length = SftBlockIdx + 4;
                    // Zero lower shifted blocks.
                    memset(Res.Blocks, 0, SftBlockIdx * sizeof(u32));
                    // Trim the leading zeros.
                    Res.TrimLeadingZeros();
                    return Res;
                }
            } else {
                BigIntFD Pow5BigInt = GetBigIntFromPow5(P5);
                if (B2 == 0) {
                    Pow5BigInt = Pow5BigInt.Multiply(B1);
                } else {
                    Pow5BigInt = Pow5BigInt.Multiply(B1, B2);
                }

                return Pow5BigInt.LeftShift(P2);
            }
        } else if (P2 != 0) {
            if (SftModIdx == 0) {
                // Shift is block aligned so can optimize by
                // just moving the values over the correct number
                // of blocks.
                return BigIntFD(B1, B2, SftBlockIdx);
            } else {
                return BigIntFD(
                    B1 << SftModIdx,
                    (B2 << SftModIdx) | (B1 >> (32 - SftModIdx)),
                    B2 >> (32 - SftModIdx),
                    SftBlockIdx
                );
            }
        } else {
            return BigIntFD(B1, B2, 0);
        }
    }


    void InitializeCache() {
        // TODO: This is copying the memory this should be calling std::move!!!
        for (ulen i = 0; i < POW5_TABLE_SIZE; i++) {
            BigIntFD BigPow5;
            BigPow5.Blocks = new u32[1];
            BigPow5.Length = 1;
            *BigPow5.Blocks = POW5_TABLE[i];
            POW5_CACHE[i] = BigPow5;
        }
        // TODO: We are also making uneeded copying of every previous value.
        BigIntFD PrevPow5 = POW5_CACHE[POW5_TABLE_SIZE - 1];
        for (ulen i = POW5_TABLE_SIZE; i < MAX_POW5_CACHE; i++) {
            PrevPow5 = PrevPow5.Multiply(5);
            POW5_CACHE[i] = PrevPow5;
        }
    }

    struct ParseData {
        FloatParseError Error;
        char*           Digits = nullptr;
        i64             DecExp;
        ulen            IValueNDigits; // Digit count for the IValue
        ulen            DigitCount;    // Digit count for digits that cannot fit into IValue
        ulen            NDigits;       // DigitCount + IValueNDigits
        u64             IValue;
    };

    ParseData ParseFloatingNumber(llvm::StringRef Text) {
        
        ulen Len = Text.size();

        // Consuming leading zeros.
        //
        ulen Idx = 0;
        ulen NLeadingZeros = 0;
        bool SeenDecPt = false;
        ulen DecPt = 0;
        ulen SeperatorCount = 0, SeperatorCountBeforeDot = 0;
        while (Idx < Len) {
            char C = Text[Idx];
            if (C == '0') {
                ++NLeadingZeros;
            } else if (C == NUMBER_SEPERATOR) {
                ++SeperatorCount;
            } else if (C == '.') {
                // Do not break here because there may
                // still be zeros after the decimal point.
                //
                // Ex.  0000.00001   has 0 leading zeros.
                SeperatorCountBeforeDot = SeperatorCount;
                DecPt     = Idx;
                SeenDecPt = true;
            } else {
                break;
            }
            ++Idx;
        }

        // Continue consuming digits as long as
        // they can fit into the integer.
    
        u64  IValue = 0;
        ulen ZeroTrailCount = 0;
        ulen IValueNDigits = 0;
        while (IValueNDigits+ZeroTrailCount < MAX_DOUBLE_DECIMAL_DIGITS && Idx < Len) {
            char C = Text[Idx];
            if (C >= 49 && C <= 57) { // Digits non-inclusive of 0.
                if (ZeroTrailCount != 0) {
                    // The digits were not actually trailing!
                    IValueNDigits += ZeroTrailCount;
                    IValue *= POW10_INT64_TABLE[ZeroTrailCount];
                    ZeroTrailCount = 0;
                }
                IValue *= 10;
                IValue += static_cast<decltype(IValue)>(C) - '0';
                ++IValueNDigits;
            } else if (C == '0') {
                // Could be trailing.
                ++ZeroTrailCount;
            } else if (C == NUMBER_SEPERATOR) {
                ++SeperatorCount;
            } else if (C == '.') {
                SeperatorCountBeforeDot = SeperatorCount;
                SeenDecPt = true;
                DecPt     = Idx;
            } else {
                break;
            }
            ++Idx;
        }
    

        // May still place some of the trailing zeros
        // into the IValue if the zeros are not all trailing.
        //
        // Because they may all be trailing we have to continue looping that
        // is the reason for this instead of just adding them directly to the IValue.
        if (ZeroTrailCount != 0 && IValueNDigits < MAX_DOUBLE_DECIMAL_DIGITS) {
            // Need to determine if the remaining digits are all
            // trailing or not!
            while (Idx < Len) {
                char C = Text[Idx];
                if (C >= 49 && C <= 57) { // Digits non-inclusive of 0.
                    // Okay so the remaining digits are not trailing zeros!
                    ulen TakenZeros = MAX_DOUBLE_DECIMAL_DIGITS - IValueNDigits;
                    IValue *= POW10_INT64_TABLE[TakenZeros];
                    IValueNDigits = MAX_DOUBLE_DECIMAL_DIGITS;
                    ZeroTrailCount -= TakenZeros;
                    break;
                } else if (C == '0') {
                    ++ZeroTrailCount;
                } else if (C == NUMBER_SEPERATOR) {
                    ++SeperatorCount;
                } else if (C == '.') {
                    SeperatorCountBeforeDot = SeperatorCount;
                    SeenDecPt = true;
                    DecPt     = Idx;
                } else {
                    break;
                }
                ++Idx;
            }
        }

        // Create a small digit buffer that does not contain the seperators
        // to make the calculations later on a bit easier.
        ulen DigitCount = 0;
        ulen BufferSize = Len - Idx + ZeroTrailCount + 1;
        char* Digits = Idx < Len ? new char[BufferSize] : nullptr;
        if (Digits) {
            memset(Digits, '0', BufferSize);
        }
        while (Idx < Len) {
            char C = Text[Idx];
            if (C >= 49 && C <= 57) { // Digits non-inclusive of 0.
                if (ZeroTrailCount != 0) {
                    // The digits were not actually trailing!
                    DigitCount += ZeroTrailCount;
                    ZeroTrailCount = 0;
                }
                ZeroTrailCount = 0;
                Digits[DigitCount++] = C;
            } else if (C == '0') {
                // Could be trailing.
                // memset to zero  Digits[DigitCount + ZeroTrailCount] = '0';
                ++ZeroTrailCount;
            } else if (C == NUMBER_SEPERATOR) {
                ++SeperatorCount;
            } else if (C == '.') {
                SeperatorCountBeforeDot = SeperatorCount;
                SeenDecPt = true;
                DecPt     = Idx;
            } else {
                break;
            }
            ++Idx;
        }

        ulen NDigits = DigitCount + IValueNDigits;

        i64 DecExp = 0;
        if (SeenDecPt) {
            DecExp = DecPt - SeperatorCountBeforeDot - NLeadingZeros;
        } else {
            DecExp = NDigits + ZeroTrailCount;
        }
    
    
        // Parsing the optional exponent.
        if (Idx < Len && (Text[Idx] == 'E' || Text[Idx] == 'e')) {
            ++Idx;
            i64 ExpSign = 1;
            if (Text[Idx] == '+') {
                ++Idx;
            } else if (Text[Idx] == '-') {
                ExpSign = -1;
                ++Idx;
            }
            // limit to i32 maximum which is way more than enough.
            constexpr i64 OverflowAmount = 2147483647 / 10;
            i64 ExpValue = 0;
            while (Idx < Len) {
                char C = Text[Idx];
                if (IsDigit(C)) {
                    if (ExpValue >= OverflowAmount) {
                        // The next digit will cause overflow or underflow!
                        if (ExpSign == 1) {
                            return ParseData {
                                FloatParseError::OVERFLOWED,
                                Digits
                            };
                        } else {
                            return ParseData {
                                FloatParseError::UNDERFLOWED,
                                Digits
                            };
                        }
                    }
                    ExpValue *= 10;
                    ExpValue += static_cast<u64>(C) - '0';
                    ++Idx;
                } else {
                    break;
                }
            }

            // Want to do exponent overflow checks here since just adding
            // the value when it overflows could cause the later stages to
            // not know that there was an overflow when the number overflows.
            i64 ExpLimit = 324 + NDigits + ZeroTrailCount;
            if (ExpValue > ExpLimit) {
                if (ExpSign == 1 && DecExp < 0 && (ExpValue + DecExp < ExpLimit)) {
                    // Actually everything since the added values don't overflow.
                    DecExp += ExpValue;
                } else {
                    // Not okay! Overflow or overflow!
                    if (ExpSign == 1) {
                        return ParseData {
                                FloatParseError::OVERFLOWED,
                                Digits
                            };
                    } else {
                        return ParseData {
                                FloatParseError::UNDERFLOWED,
                                Digits
                            };
                    }
                }
            } else {
                // Will not overflow. Although could still overflow
                // later when taking into account the digits.
                DecExp += ExpSign * ExpValue;
            }
        }

        return ParseData {
            FloatParseError::NONE,
            Digits,
            DecExp,
            IValueNDigits,
            DigitCount,
            NDigits,
            IValue
        };
    }

    
template<typename T>
struct CorrectionRoutineInfo {
    using IntTy                     = u64;
    static const IntTy MantissaSize = 0;
    static const IntTy MantissaMask = 0;
    static const i64   ExpSize      = 0;
    static const i64   ExpBias      = 0;
    static const IntTy ExpMask      = 0;
};
template<>
struct CorrectionRoutineInfo<double> {
    using IntTy                   = u64;
    static const u64 MantissaSize = 52;
    static const u64 MantissaMask = 0xFFFFFFFFFFFFF;
    static const i64 ExpSize      = 11;
    static const i64 ExpBias      = 1023;
    static const u64 ExpMask      = 0x7FF0000000000000ull;
};
template<>
struct CorrectionRoutineInfo<float> {
    using IntTy                   = u32;
    static const u32 MantissaSize = 23;
    static const u32 MantissaMask = 0x7FFFFF;
    static const i64 ExpSize      = 9;
    static const i64 ExpBias      = 127;
    static const u32 ExpMask      = 0x7F800000;
};

template<typename VTy>
VTy CorrectionRoutine(VTy Value, ParseData& ParseData) {

    // Value is now approximately the correct
    // result.
    // But what will need to be done is that an exact
    // representation of the value, generated with
    // big integers will need to be constructed. Then
    // the two big integers are compared. If they do
    // not match then corrections to the bits of the
    // estimate are made.
    
    using IntTy = CorrectionRoutineInfo<VTy>::IntTy;

    union Converge {
        VTy   F;
        IntTy I;
    };
    Converge Conv;
    Conv.F = Value;
    IntTy Bits = Conv.I;

    // How the calculations are derivated:
    //
    // "How to Read Floating Point Numbers Accurately" by
    // William D Clinger, section 5 tells us what we are
    // going to solve for. Let x denote the exact value
    // and y our approximation then consider the two formuals:
    //
    // 1:   x/y = f*10^e / (m*b^k)
    // 2:   f*10^e = (m+err)*2^k
    //
    // Our stopping condition is:   err <= 1/2.
    // Solving for err you get:
    //
    // err = m(x - y)/y
    // plugging into the inequality:
    // m(x - y)/y <= 1/2  ->  2m(x - y) <= y
    //                    ->  2m*x - 2m*y <= y
    //                    ->  2m*f*10^e - 2m*m*2^k <= m*2^k
    //                    ->  2*f*10^e - m*2^(k+1) <= 2^k
    //                    ->  2*f*(5^e)*(2^e) - m*2^(k+1) <= 2^k
    //                    ->  f*(5^e)*(2^(e+1)) - m*2^(k+1) <= 2^k
    //
    // However to do big integer comparisons only 5^e, 2^(e+1),
    // 2^(k+1), and 2^k must be moved to the respective sides of
    // the inequalities as to remove any possible division.
    //
    // Once that is done then common factors of 2 can be factored
    // out of each powers of 2 to reduce computations at which point
    // the comparisons can be made with big integers.


    // Want to get X,Y a form s.t. it has no fractional
    // part for comparisons and no trailing zeros so
    // CExp is used to adjust.
    //
    // Ex. '0.000000000000000000000000003315'
    // would need to shift 30 times s.t. the value
    // becomes '3315'.
    // Ex. '523643632423445765462344213213250000'
    // would need to shift 4 times to remove the zeros
    // and becomes '52364363242344576546234421321325'.
    i64 CExp = ParseData.DecExp - ParseData.NDigits;
    i64 Y5 = MAXX(0, -CExp);
    i64 X5 = MAXX(0, +CExp);

    BigIntFD ExactBigIntP5 =
        CreateBigIntFromDecimalDigits(ParseData.IValue,
                                      ParseData.Digits,
                                      ParseData.DigitCount,
                                      ParseData.NDigits);
    
    BigIntFD ExactBigInt;
    bool ConstructedExactBigInt = false;
    if (X5 != 0) {
        ExactBigIntP5 = ExactBigIntP5.MultiplyPow5(X5);
    }

    // In case the same X2 is found the next iteration of the loop
    // we want to prevent having to reconstruct the ExactBigInt.
    i64 PrevX2 = 0;

    constexpr IntTy MantissaSize = CorrectionRoutineInfo<VTy>::MantissaSize;
    constexpr IntTy MantissaMask = CorrectionRoutineInfo<VTy>::MantissaMask;
    constexpr i64   ExpSize      = CorrectionRoutineInfo<VTy>::ExpSize;
    constexpr i64   ExpBias      = CorrectionRoutineInfo<VTy>::ExpBias;
    constexpr IntTy ExpMask      = CorrectionRoutineInfo<VTy>::ExpMask;
    
    // Now for correction.
    while (true) {
        i64   EBits = static_cast<i64>(Bits >> MantissaSize);
        IntTy MBits = Bits & MantissaMask;
        if (EBits > 0) { // Normalized.
            MBits |= (static_cast<IntTy>(1) << MantissaSize); // Add the implicit 1 to the mantissa.
        } else { // Going to normalize the denormalized numbers.
            // To normalize we shift the first 1 in the mantissa
            // until its in the appropriate 53 bit index.
            ulen LeadingZeros = GetNumberOfLeadingZeros(MBits);
            // -ExpSize since GetNumberOfLeadingZeros also returns the exponent
            // zeros.
            ulen Shift = LeadingZeros - ExpSize; 
            MBits <<= Shift;
            EBits = 1 - static_cast<i64>(Shift);
        }
        EBits -= ExpBias; // Remove the bias
        ulen TrailZerosCount = GetNumberOfTrailingZeros(MBits);
        // Want to remove lower order bits so that 2^TrailZerosCount
        // may be factored out in the comparison to ExactBigInt.
        MBits >>= static_cast<u64>(TrailZerosCount);
        i64 UExp = EBits - MantissaSize + TrailZerosCount; // Pull out the trailing zeros for potential common factor.

        // The 10^5 is split between powers of 2 and powers of
        // 5 respectively.
        i64 Y2 = Y5;
        i64 X2 = X5;
        if (UExp >= 0) {
            Y2 += UExp;
        } else {
            X2 += -UExp;
        }
        i64 CmpRHS2 = Y2;
        
        i64 ErrLHSCmp;
        if (EBits <= -ExpBias) {
            ErrLHSCmp = EBits + ExpBias + TrailZerosCount;
        } else {
            ErrLHSCmp = 1 + TrailZerosCount;
        }
        Y2 += ErrLHSCmp;
        X2 += ErrLHSCmp;
        
        i64 commomFactorOf2 = MINN(Y2, MINN(X2, CmpRHS2));
        Y2      -= commomFactorOf2;
        X2      -= commomFactorOf2;
        CmpRHS2 -= commomFactorOf2;

        BigIntFD EstBigInt = GetBigIntFromSigfPow2Pow5(MBits, Y5, Y2);
        if (!ConstructedExactBigInt || PrevX2 != X2) {
            ExactBigInt = ExactBigIntP5.LeftShift(X2);
            ConstructedExactBigInt = true;
            PrevX2 = X2;
        }

        // Now we compare the results to see if the estimated
        // value matches the exact value.
        BigIntFD Difference;
        i64 CompareRes = EstBigInt.Compare(ExactBigInt);
        bool TooBig;
        if (CompareRes > 0) {
            // The estimated value was too big!
            TooBig = true;
            Difference = EstBigInt.Subtract(ExactBigInt);
            if (TrailZerosCount == MantissaSize && EBits > -ExpBias + 1) {
                // The estimate is an exact power of 2 and not
                // denormalized.
                // Lies on exact powers of 2 so must take into account.
                CmpRHS2 -= 1;
                if (CmpRHS2 < 0) {
                    CmpRHS2 = 0;
                    Difference = Difference.LeftShift(1);
                }
            }
        } else if (CompareRes < 0) {
            // The estimated value was too small!
            TooBig = false;
            Difference =  ExactBigInt.Subtract(EstBigInt);
        } else {
            // Perfect match! Ending correction!
            break;
        }
        
        // Not an exact match but may be less than the allowed error.
        BigIntFD RHSCmp = GetBigIntFromPow5(Y5).LeftShift(CmpRHS2);
        CompareRes = Difference.Compare(RHSCmp);
        if (CompareRes < 0) {
            // The difference is less than 1/2
            // so it's close enough!
            break;
        } else if (CompareRes == 0) {
            // The difference is exactly 1/2
            // so going to round to nearest and
            // call that the result!
            if ((Bits & 1) != 0) {
                Bits += TooBig ? -1 : +1;
            }
            break;
        } else {
            // The difference is too much! Must
            // correct the Bits and try again!
            Bits += TooBig ? -1 : +1;
            if (Bits == 0 || Bits == ExpMask) {
                // Correction led to possitive infity or 0
                // so ending the correction loop and returning
                // one of those results.
                break;
            }
        }
    }

    // Bitcast the corrected bits to a double
    // and return the result to the user.
    Conv.I = Bits;
    return Conv.F;
}


#define RETURN_AND_CLEANUP(V) \
delete ParseData.Digits;      \
return V;

    double ToIEEEDouble(llvm::StringRef Text, FloatParseError& Error) {
        Error = FloatParseError::NONE;

        ParseData ParseData = ParseFloatingNumber(Text);
        u64 IValue = ParseData.IValue;
        double Value = static_cast<double>(IValue);
        i64 E = ParseData.DecExp - static_cast<i64>(ParseData.IValueNDigits);
        
        if (ParseData.Error != FloatParseError::NONE) {
            Error = ParseData.Error;
            RETURN_AND_CLEANUP(0.0);
        }

        // According to "How to Read Floating Point Numbers Accurately" by
        // William D Clinger under section 7, as long as certain constraints
        // are met exact conversions can be made right away.
        if (ParseData.NDigits < MAX_DOUBLE_DECIMAL_DIGITS) {
        
            if (E == 0 || IValue == 0) {
                RETURN_AND_CLEANUP(Value);
            } else if (E >= 0) {
                if (E <= DOUBLE_POW10_TABLE_LIMIT) {
                    Value *= POW10_DOUBLE_TABLE[E];
                    RETURN_AND_CLEANUP(Value);
                }
                i64 Slop = MAX_DOUBLE_DECIMAL_DIGITS - ParseData.IValueNDigits - 1;
                if (E <= DOUBLE_POW10_TABLE_LIMIT + Slop) {
                    Value *= POW10_DOUBLE_TABLE[Slop];
                    Value *= POW10_DOUBLE_TABLE[E - Slop];
                    RETURN_AND_CLEANUP(Value);
                }

                // Else continue below for harder case
            } else {
                if (E >= -DOUBLE_POW10_TABLE_LIMIT) {
                    Value /= POW10_DOUBLE_TABLE[-E];
                    RETURN_AND_CLEANUP(Value);
                }

                // Else continue below for harder case
            }
        }

        // Could not easily compute.
        // 
        // An approximation is made at first
        // and then later its bits get used to
        // construct a big integer which is compared
        // against another big integer with exact values.
        // At that time the bits are compared and
        // corrections are made.

        if (E > 0) {
            if (ParseData.DecExp > 309) {
                Error = FloatParseError::OVERFLOWED;
                RETURN_AND_CLEANUP(0.0);
            }
            if ((E & 15) != 0) {
                Value *= POW10_DOUBLE_TABLE[E & 15];
            }
            E >>= 4;
            if (E != 0) {
                ulen i = 0;
                for (; E > 1; i++, E >>= 1) {
                    if ((E & 1) != 0) {
                        Value *= POW10_DOUBLE_BIG_TABLE[i];
                    }
                }

                double ValuePotOverflow = Value * POW10_DOUBLE_BIG_TABLE[i];
                // Can overflow for max values so go do check.
                // TODO: is relying on std::isinf a good idea?
                if ( std::isinf(ValuePotOverflow) ) {
                    ValuePotOverflow = Value / 2.0;
                    ValuePotOverflow *= POW10_DOUBLE_BIG_TABLE[i];
                    if (std::isinf(ValuePotOverflow)) {
                        // Okay, guess it was actually infinity.
                        Error = FloatParseError::OVERFLOWED;
                        RETURN_AND_CLEANUP(0.0);
                    } // else not infinity just max value
                    ValuePotOverflow = std::numeric_limits<double>::max();
                }
                Value = ValuePotOverflow;
            }
        } else if (E < 0) {
            E = -E;
            if (ParseData.DecExp < -325) {
                Error = FloatParseError::UNDERFLOWED;
                RETURN_AND_CLEANUP(0.0);
            }
            if ((E & 15) != 0) {
                Value /= POW10_DOUBLE_TABLE[E & 15];
            }
            E >>= 4;
            if (E != 0) {
                ulen i = 0;
                for (; E > 1; i++, E >>= 1) {
                    if ((E & 1) != 0) {
                        Value *= POW10_DOUBLE_TINY_TABLE[i];
                    }
                }

                double ValuePotUnderflow = Value * POW10_DOUBLE_TINY_TABLE[i];
                if (ValuePotUnderflow == 0.0) {
                    ValuePotUnderflow = Value * 2.0;
                    ValuePotUnderflow *= POW10_DOUBLE_TINY_TABLE[i];
                    if (ValuePotUnderflow == 0.0) {
                        Error = FloatParseError::UNDERFLOWED;
                        RETURN_AND_CLEANUP(0.0);
                    }
                    ValuePotUnderflow = std::numeric_limits<double>::denorm_min();
                }
                Value = ValuePotUnderflow;
            }
        }

        double ResultValue = CorrectionRoutine<double>(Value, ParseData);
        if (ResultValue == 0.0) {
            Error = FloatParseError::UNDERFLOWED;
        } else if (ResultValue == std::numeric_limits<double>::infinity()) {
            Error = FloatParseError::OVERFLOWED;
        }
        RETURN_AND_CLEANUP(ResultValue);
    }

    // Most comments are simplified here. Read ToIEEEDouble for
    // more fleshed out details.
    float ToIEEESingle(llvm::StringRef Text, FloatParseError& Error) {
        Error = FloatParseError::NONE;

        ParseData ParseData = ParseFloatingNumber(Text);
        u64 IValue = ParseData.IValue;
        i64 E = ParseData.DecExp - static_cast<i64>(ParseData.IValueNDigits);

        if (ParseData.Error != FloatParseError::NONE) {
            Error = ParseData.Error;
            RETURN_AND_CLEANUP(0.0);
        }

        // Maybe its easy case and can return immediately.
        if (ParseData.NDigits < MAX_SINGLE_DECIMAL_DIGITS) {
            float SingleValue = static_cast<float>(IValue);
            if (E == 0 && IValue == 0) {
                RETURN_AND_CLEANUP(SingleValue);
            } else if (E >= 0) {
                if (E <= SINGLE_POW10_TABLE_LIMIT) {
                    SingleValue *= POW10_SINGLE_TABLE[E];
                    RETURN_AND_CLEANUP(SingleValue);
                }
                i64 Slop = MAX_SINGLE_DECIMAL_DIGITS - ParseData.IValueNDigits - 1;
                if (E <= SINGLE_POW10_TABLE_LIMIT + Slop) {
                    SingleValue *= POW10_SINGLE_TABLE[Slop];
                    SingleValue *= POW10_SINGLE_TABLE[E - Slop];
                    RETURN_AND_CLEANUP(SingleValue);
                }
                // Else continue below for harder case
            } else {
                if (E >= -SINGLE_POW10_TABLE_LIMIT) {
                    SingleValue /= POW10_SINGLE_TABLE[-E];
                    RETURN_AND_CLEANUP(SingleValue);
                }

                // Else continue below for harder case
            }
        } else if (ParseData.DecExp >= static_cast<i64>(ParseData.NDigits) &&
                   (ParseData.NDigits + ParseData.DecExp) <= MAX_DOUBLE_DECIMAL_DIGITS - 1) {
            double DoubleValue = static_cast<double>(IValue);
            DoubleValue *= POW10_DOUBLE_TABLE[E];
            RETURN_AND_CLEANUP(static_cast<float>(DoubleValue));
        }

        // Creating approximation.
        double DoubleValue = static_cast<double>(IValue);
        if (E > 0) {
            if (ParseData.DecExp > 39) {
                Error = FloatParseError::OVERFLOWED;
                RETURN_AND_CLEANUP(0.0);
            }
            if ((E & 15) != 0) {
                DoubleValue *= POW10_DOUBLE_TABLE[E & 15];
            }
            E >>= 4;
            if (E != 0) {
                ulen i = 0;
                for (; E > 0; i++, E >>= 1) {
                    if ((E & 1) != 0) {
                        DoubleValue *= POW10_DOUBLE_BIG_TABLE[i];
                    }
                }
            }
        } else if (E < 0) {
            E = -E;
            if (ParseData.DecExp < -46) {
                Error = FloatParseError::UNDERFLOWED;
                RETURN_AND_CLEANUP(0.0f);
            }
            if ((E & 15) != 0) {
                DoubleValue /= POW10_DOUBLE_TABLE[E & 15];
            }
            E >>= 4;
            if (E != 0) {
                ulen i = 0;
                for (; E > 0; i++, E >>= 1) {
                    if ((E & 1) != 0) {
                        DoubleValue *= POW10_DOUBLE_TINY_TABLE[i];
                    }
                }
            }
        }
        // This used to be wrong. C++ std::numeric_limits<float>::min() returns the normalized minimum
        // which is not the minimum! Why they choose this convention I have no idea but denorm_min gives
        // us the actual smallest possible value.
        float SingleValue = MAXX(std::numeric_limits<float>::denorm_min(),
                            MINN(std::numeric_limits<float>::max(), static_cast<float>(DoubleValue)));

        float ResultValue = CorrectionRoutine<float>(SingleValue, ParseData);
        if (ResultValue == 0.0f) {
            Error = FloatParseError::UNDERFLOWED;
        } else if (ResultValue == std::numeric_limits<float>::infinity()) {
            Error = FloatParseError::OVERFLOWED;
        }
        RETURN_AND_CLEANUP(ResultValue);
    }

#undef RETURN_AND_CLEANUP

}
}