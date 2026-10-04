-- Seed: 10864621977129357007,15795020531041709203

library ieee;
use ieee.std_logic_1164.all;

entity ehtnwfmun is
  port (unsklrvq : out std_logic_vector(2 to 1); xzg : inout integer; ep : inout std_logic_vector(3 to 2); uxlpmkk : buffer bit_vector(0 downto 2));
end ehtnwfmun;

architecture ksavqmbw of ehtnwfmun is
  
begin
  -- Single-driven assignments
  uxlpmkk <= (others => '0');
  
  -- Multi-driven assignments
  ep <= "";
  unsklrvq <= ep;
  ep <= (others => '0');
  ep <= (others => '0');
end ksavqmbw;

library ieee;
use ieee.std_logic_1164.all;

entity ldsyuyiiv is
  port (wylwqgkue : linkage severity_level; tywfnvcg : out time; ok : out real; qwavjvzo : linkage std_logic);
end ldsyuyiiv;

library ieee;
use ieee.std_logic_1164.all;

architecture faxew of ldsyuyiiv is
  signal jsmfajh : bit_vector(0 downto 2);
  signal nscnsnm : integer;
  signal odtg : std_logic_vector(2 to 1);
  signal vxouo : bit_vector(0 downto 2);
  signal qrs : std_logic_vector(3 to 2);
  signal uavucmadzt : integer;
  signal lmzbujm : std_logic_vector(3 to 2);
begin
  vmgbqtc : entity work.ehtnwfmun
    port map (unsklrvq => lmzbujm, xzg => uavucmadzt, ep => qrs, uxlpmkk => vxouo);
  lw : entity work.ehtnwfmun
    port map (unsklrvq => odtg, xzg => nscnsnm, ep => lmzbujm, uxlpmkk => jsmfajh);
  
  -- Multi-driven assignments
  lmzbujm <= "";
  odtg <= "";
  lmzbujm <= lmzbujm;
end faxew;



-- Seed after: 16689809638343459600,15795020531041709203
