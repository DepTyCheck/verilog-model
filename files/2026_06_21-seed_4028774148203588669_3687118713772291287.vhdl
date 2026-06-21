-- Seed: 4028774148203588669,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity sg is
  port (hq : linkage time_vector(3 downto 0); rbg : linkage std_logic; fjlh : in std_logic_vector(2 to 2); gf : linkage time);
end sg;

architecture jsibo of sg is
  
begin
  
end jsibo;

library ieee;
use ieee.std_logic_1164.all;

entity ykhfowprfa is
  port (tonmlm : linkage std_logic_vector(4 to 0));
end ykhfowprfa;

library ieee;
use ieee.std_logic_1164.all;

architecture x of ykhfowprfa is
  signal cyzndq : time;
  signal fyycpmwn : std_logic_vector(2 to 2);
  signal bbhvtzcs : time_vector(3 downto 0);
  signal mtubcw : time;
  signal qsfjbodj : std_logic_vector(2 to 2);
  signal yxuvnhh : time_vector(3 downto 0);
  signal khdq : time;
  signal nuswnytspv : std_logic_vector(2 to 2);
  signal vroy : std_logic;
  signal ltojv : time_vector(3 downto 0);
begin
  ff : entity work.sg
    port map (hq => ltojv, rbg => vroy, fjlh => nuswnytspv, gf => khdq);
  vmho : entity work.sg
    port map (hq => yxuvnhh, rbg => vroy, fjlh => qsfjbodj, gf => mtubcw);
  fctvmscd : entity work.sg
    port map (hq => bbhvtzcs, rbg => vroy, fjlh => fyycpmwn, gf => cyzndq);
end x;

entity kqyslkzwok is
  port (tuuc : inout real; qdbuassm : linkage character);
end kqyslkzwok;

library ieee;
use ieee.std_logic_1164.all;

architecture wopgtzo of kqyslkzwok is
  signal szqvgeucl : time;
  signal cbmgqr : std_logic_vector(2 to 2);
  signal ntqv : std_logic;
  signal jdlfyox : time_vector(3 downto 0);
  signal jisdkj : std_logic_vector(4 to 0);
begin
  nwjihshpy : entity work.ykhfowprfa
    port map (tonmlm => jisdkj);
  ykzotb : entity work.sg
    port map (hq => jdlfyox, rbg => ntqv, fjlh => cbmgqr, gf => szqvgeucl);
  
  -- Single-driven assignments
  tuuc <= 2_4_0_4_0.01034;
end wopgtzo;



-- Seed after: 3333799788680928132,3687118713772291287
