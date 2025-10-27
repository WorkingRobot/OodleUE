// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.

		if (packet >= 0x10) // medium LRL + RL
		{
			pkts -= 2;
			U32 v = RR_GET16_LE_UNALIGNED(pkts) - 0x1000;
			UINTa lrl = v & 0x3f;
			UINTa rl = v >> 6;

			// copy <64 bytes (LRL)
			write128(outp + 0x00, read128(lits + 0x00));
			write128(outp + 0x10, read128(lits + 0x10));
			write128(outp + 0x20, read128(lits + 0x20));
			write128(outp + 0x30, read128(lits + 0x30));
			outp += lrl;
			lits += lrl;

			// store <128 bytes (RL)
			// start with first 48 bytes unaligned
			write128(outp + 0x00, runv);
			write128(outp + 0x10, runv);
			write128(outp + 0x20, runv);

			// write rest aligned (if present)
			if (rl > 48)
			{
				U8 *outp_a = (U8 *)(((UINTa)outp + 15) & ~15);
				// need to write +0x20 again since we skipped ahead by up to 15 bytes
				write128a(outp_a + 0x20, runv);
				write128a(outp_a + 0x30, runv);
				write128a(outp_a + 0x40, runv);
				write128a(outp_a + 0x50, runv);
				write128a(outp_a + 0x60, runv);
				write128a(outp_a + 0x70, runv);
			}

			outp += rl;
		}
		else if (packet == 1) // change run value
		{
			--pkts;
			runv = splatb_128(lits);
			++lits;
		}
		else if (packet >= 0x09) // long RL
		{
			pkts -= 2;
			U32 v = RR_GET16_LE_UNALIGNED(pkts) - 0x900 + 1;

			// check we have sufficient output space left
			SINTa rl = v << 7;
			if (out_end - outp < rl)
  			{
  				NEWLZ_ARRAY_RETURN_FAILURE();
  			}

			// write last 64 bytes precisely, plus one 16b block at the
			// start to get us to alignment
			write128(outp, runv);
			U8 *outp_a = (U8 *)(((UINTa)outp + 15) & ~15);

			outp += rl;
			write128(outp - 0x40, runv);
			write128(outp - 0x30, runv);
			write128(outp - 0x20, runv);
			write128(outp - 0x10, runv);

			// write rest aligned
			U8 *outp_end = outp - 0x40;
			do
			{
				write128a(outp_a + 0x00, runv);
				write128a(outp_a + 0x10, runv);
				write128a(outp_a + 0x20, runv);
				write128a(outp_a + 0x30, runv);
				outp_a += 0x40;
			} while (outp_a < outp_end);
		}
		else /*if (packet >= 0x02)*/ // long LRL (only code left)
		{
			RR_ASSERT(packet >= 0x02);
			pkts -= 2;
			U32 v = RR_GET16_LE_UNALIGNED(pkts) - 0x200 + 1;

			// check we have sufficient literal bytes and output space left
			SINTa lrl = v << 6;
			if (pkts - lits < lrl || out_end - outp < lrl)
  			{
  				NEWLZ_ARRAY_RETURN_FAILURE();
  			}

			// write first 16 bytes precisely
			write128(outp, read128(lits));

			// get to 16-byte output alignment
			UINTa align_amt = (0 - UINTa(outp)) & 15;
			U8 *outp_a = outp + align_amt;
			const U8 *lits_a = lits + align_amt;

			// now write remaining 48 bytes of first block aligned
			write128a(outp_a + 0x00, read128(lits_a + 0x00));
			write128a(outp_a + 0x10, read128(lits_a + 0x10));
			write128a(outp_a + 0x20, read128(lits_a + 0x20));

			// aligned bulk loop
			while (--v)
			{
				write128a(outp_a + 0x30, read128(lits_a + 0x30));
				write128a(outp_a + 0x40, read128(lits_a + 0x40));
				write128a(outp_a + 0x50, read128(lits_a + 0x50));
				write128a(outp_a + 0x60, read128(lits_a + 0x60));
				outp_a += 0x40;
				lits_a += 0x40;
			}

			outp += lrl;
			lits += lrl;

			// write last 16 bytes precisely
			write128(outp - 0x10, read128(lits - 0x10));
		}

