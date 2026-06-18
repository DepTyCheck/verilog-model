-- Seed: 13573676862147727567,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (fmwfz : in boolean; erl : linkage std_logic_vector(3 downto 4); iqumihjay : inout integer; yuiqpcaftk : out boolean);
end f;

architecture uoqprser of f is
  
begin
  -- Single-driven assignments
  yuiqpcaftk <= FALSE;
  iqumihjay <= 13320;
end uoqprser;

library ieee;
use ieee.std_logic_1164.all;

entity teuoe is
  port (tjhdezv : linkage std_logic_vector(1 to 0); yewcefxo : in std_logic_vector(1 to 0); m : out severity_level; lvytk : linkage severity_level);
end teuoe;

library ieee;
use ieee.std_logic_1164.all;

architecture l of teuoe is
  signal wrabfjvcsf : integer;
  signal cazsd : boolean;
  signal c : integer;
  signal mruyxml : std_logic_vector(3 downto 4);
  signal nwihfbmcxc : boolean;
begin
  pqquqy : entity work.f
    port map (fmwfz => nwihfbmcxc, erl => mruyxml, iqumihjay => c, yuiqpcaftk => cazsd);
  unwcequ : entity work.f
    port map (fmwfz => nwihfbmcxc, erl => yewcefxo, iqumihjay => wrabfjvcsf, yuiqpcaftk => nwihfbmcxc);
  
  -- Single-driven assignments
  m <= NOTE;
  
  -- Multi-driven assignments
  mruyxml <= "";
  mruyxml <= (others => '0');
end l;



-- Seed after: 14551766991147154805,8118127366649987907
