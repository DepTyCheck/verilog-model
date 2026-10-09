-- Seed: 13585507383209125437,8891552411914730853

entity yeslydwb is
  port (gfhetokgoh : linkage time; d : buffer integer_vector(3 downto 0); nhb : buffer severity_level);
end yeslydwb;

architecture bzkgr of yeslydwb is
  
begin
  
end bzkgr;

library ieee;
use ieee.std_logic_1164.all;

entity ogabzt is
  port (juxkoqtcj : out std_logic; p : buffer integer; nxfjpbcnbh : in time);
end ogabzt;

architecture azb of ogabzt is
  signal uuomc : severity_level;
  signal z : integer_vector(3 downto 0);
  signal yyyxpeaohi : time;
  signal vkl : severity_level;
  signal yehlrwhkax : integer_vector(3 downto 0);
  signal cwm : time;
begin
  srbefbpri : entity work.yeslydwb
    port map (gfhetokgoh => cwm, d => yehlrwhkax, nhb => vkl);
  o : entity work.yeslydwb
    port map (gfhetokgoh => yyyxpeaohi, d => z, nhb => uuomc);
  
  -- Single-driven assignments
  p <= 8#303#;
  
  -- Multi-driven assignments
  juxkoqtcj <= 'L';
  juxkoqtcj <= juxkoqtcj;
end azb;

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (lb : linkage std_logic; yuoh : in integer_vector(3 to 2); wiyxfqbuws : out integer);
end d;

library ieee;
use ieee.std_logic_1164.all;

architecture rbddvpqk of d is
  signal vujuplc : integer;
  signal ifpxyqlvni : std_logic;
  signal u : severity_level;
  signal jtykmn : integer_vector(3 downto 0);
  signal pmhchf : time;
begin
  hl : entity work.yeslydwb
    port map (gfhetokgoh => pmhchf, d => jtykmn, nhb => u);
  dxlx : entity work.ogabzt
    port map (juxkoqtcj => ifpxyqlvni, p => vujuplc, nxfjpbcnbh => pmhchf);
  
  -- Single-driven assignments
  wiyxfqbuws <= 2#0_1_1_1_1#;
end rbddvpqk;



-- Seed after: 3824610478884694415,8891552411914730853
