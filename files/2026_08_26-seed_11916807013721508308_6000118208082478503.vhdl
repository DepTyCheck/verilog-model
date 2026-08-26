-- Seed: 11916807013721508308,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity mfmz is
  port (l : linkage real; ccuzlci : inout std_logic_vector(3 downto 2); mmcnzvs : linkage real_vector(3 downto 0); ymtaxdy : buffer std_logic);
end mfmz;

architecture rjlnirst of mfmz is
  
begin
  -- Multi-driven assignments
  ccuzlci <= "WH";
  ymtaxdy <= 'U';
  ymtaxdy <= ymtaxdy;
end rjlnirst;

entity xs is
  port (wca : inout time);
end xs;

library ieee;
use ieee.std_logic_1164.all;

architecture a of xs is
  signal sddhk : real_vector(3 downto 0);
  signal txsea : real;
  signal egaokxbf : real_vector(3 downto 0);
  signal eahzzoeogr : real;
  signal jivo : std_logic;
  signal dg : real_vector(3 downto 0);
  signal xjecgaas : std_logic_vector(3 downto 2);
  signal hecfqzwv : real;
begin
  pubiwj : entity work.mfmz
    port map (l => hecfqzwv, ccuzlci => xjecgaas, mmcnzvs => dg, ymtaxdy => jivo);
  em : entity work.mfmz
    port map (l => eahzzoeogr, ccuzlci => xjecgaas, mmcnzvs => egaokxbf, ymtaxdy => jivo);
  zzhihigcb : entity work.mfmz
    port map (l => txsea, ccuzlci => xjecgaas, mmcnzvs => sddhk, ymtaxdy => jivo);
  
  -- Single-driven assignments
  wca <= 0_4_1_1.4320 fs;
end a;



-- Seed after: 17876768514343433537,6000118208082478503
