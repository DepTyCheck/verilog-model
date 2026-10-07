-- Seed: 8089877874326753914,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity efby is
  port (imfxw : inout integer; eoi : in time; wqohnqsz : inout std_logic_vector(2 downto 2));
end efby;

architecture hobnh of efby is
  
begin
  -- Single-driven assignments
  imfxw <= imfxw;
  
  -- Multi-driven assignments
  wqohnqsz <= wqohnqsz;
end hobnh;

library ieee;
use ieee.std_logic_1164.all;

entity ju is
  port (ruwubz : buffer time; zaq : in std_logic_vector(0 to 2); s : in string(1 downto 4));
end ju;

architecture t of ju is
  
begin
  -- Single-driven assignments
  ruwubz <= 104 fs;
end t;

library ieee;
use ieee.std_logic_1164.all;

entity kvbnrcu is
  port (izycpqleo : inout std_logic; vb : linkage integer; ldpg : inout real_vector(4 to 4); jq : out boolean_vector(0 downto 0));
end kvbnrcu;

library ieee;
use ieee.std_logic_1164.all;

architecture hc of kvbnrcu is
  signal lz : std_logic_vector(2 downto 2);
  signal lrbyhnnybl : time;
  signal ujljyn : integer;
  signal meqv : std_logic_vector(2 downto 2);
  signal eos : integer;
  signal ginro : std_logic_vector(2 downto 2);
  signal wctvoczu : time;
  signal ink : integer;
begin
  vnohsxuua : entity work.efby
    port map (imfxw => ink, eoi => wctvoczu, wqohnqsz => ginro);
  ibjfbvwso : entity work.efby
    port map (imfxw => eos, eoi => wctvoczu, wqohnqsz => meqv);
  xcjm : entity work.efby
    port map (imfxw => ujljyn, eoi => lrbyhnnybl, wqohnqsz => lz);
  
  -- Single-driven assignments
  jq <= (others => FALSE);
  
  -- Multi-driven assignments
  izycpqleo <= '-';
  izycpqleo <= '1';
end hc;



-- Seed after: 4666340139359226209,5906004015519833893
