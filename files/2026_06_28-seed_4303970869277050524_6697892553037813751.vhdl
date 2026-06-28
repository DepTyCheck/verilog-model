-- Seed: 4303970869277050524,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity civqvwyk is
  port (wj : inout boolean_vector(1 to 4); lyijlfau : inout std_logic; zxbny : buffer time; pd : out character);
end civqvwyk;

architecture ovmfsqtl of civqvwyk is
  
begin
  -- Single-driven assignments
  wj <= (TRUE, TRUE, FALSE, FALSE);
  zxbny <= 3 sec;
  
  -- Multi-driven assignments
  lyijlfau <= '1';
  lyijlfau <= '0';
  lyijlfau <= '0';
end ovmfsqtl;

entity eyrw is
  port (rqwjewer : linkage integer_vector(0 to 4); ko : buffer real; oiyl : out bit);
end eyrw;

library ieee;
use ieee.std_logic_1164.all;

architecture roeqbpfb of eyrw is
  signal cdlukved : character;
  signal ounkg : time;
  signal dryjdac : std_logic;
  signal wujnsrh : boolean_vector(1 to 4);
begin
  exdmilek : entity work.civqvwyk
    port map (wj => wujnsrh, lyijlfau => dryjdac, zxbny => ounkg, pd => cdlukved);
  
  -- Single-driven assignments
  ko <= 1_3_2_3_2.1;
  oiyl <= '1';
  
  -- Multi-driven assignments
  dryjdac <= '0';
  dryjdac <= '-';
  dryjdac <= '1';
  dryjdac <= 'H';
end roeqbpfb;

library ieee;
use ieee.std_logic_1164.all;

entity apzentimhz is
  port (xp : buffer boolean; hknvgfvz : in std_logic);
end apzentimhz;

library ieee;
use ieee.std_logic_1164.all;

architecture ips of apzentimhz is
  signal otjw : character;
  signal frap : time;
  signal ffiwfih : boolean_vector(1 to 4);
  signal dnnqtdgi : character;
  signal teybulzjwu : time;
  signal behfx : std_logic;
  signal o : boolean_vector(1 to 4);
  signal womquyfpm : bit;
  signal diiyisy : real;
  signal vfkhwhe : integer_vector(0 to 4);
begin
  lywrktxeij : entity work.eyrw
    port map (rqwjewer => vfkhwhe, ko => diiyisy, oiyl => womquyfpm);
  twyh : entity work.civqvwyk
    port map (wj => o, lyijlfau => behfx, zxbny => teybulzjwu, pd => dnnqtdgi);
  vavcyl : entity work.civqvwyk
    port map (wj => ffiwfih, lyijlfau => behfx, zxbny => frap, pd => otjw);
  
  -- Single-driven assignments
  xp <= FALSE;
  
  -- Multi-driven assignments
  behfx <= '-';
  behfx <= '0';
  behfx <= 'Z';
end ips;



-- Seed after: 7259217219605161459,6697892553037813751
