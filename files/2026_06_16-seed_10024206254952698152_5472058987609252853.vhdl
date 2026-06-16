-- Seed: 10024206254952698152,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity dznylxw is
  port (hxtbhoisy : out boolean_vector(3 to 2); xslmhxtsj : buffer std_logic_vector(2 to 3));
end dznylxw;

architecture naqa of dznylxw is
  
begin
  -- Single-driven assignments
  hxtbhoisy <= (others => TRUE);
end naqa;

library ieee;
use ieee.std_logic_1164.all;

entity drol is
  port (cwylosu : in std_logic_vector(4 to 2); k : linkage std_logic_vector(0 downto 3); zyziwbuo : in time);
end drol;

library ieee;
use ieee.std_logic_1164.all;

architecture rtd of drol is
  signal ttmxfepy : boolean_vector(3 to 2);
  signal siihn : std_logic_vector(2 to 3);
  signal ldnqh : boolean_vector(3 to 2);
begin
  fomsxrcy : entity work.dznylxw
    port map (hxtbhoisy => ldnqh, xslmhxtsj => siihn);
  rioxckub : entity work.dznylxw
    port map (hxtbhoisy => ttmxfepy, xslmhxtsj => siihn);
  
  -- Multi-driven assignments
  siihn <= "ZZ";
  siihn <= "XL";
  siihn <= "U1";
  siihn <= "ZW";
end rtd;

entity duzlhppg is
  port (wevbwhbq : out bit_vector(2 downto 3));
end duzlhppg;

library ieee;
use ieee.std_logic_1164.all;

architecture fdboyonkip of duzlhppg is
  signal qmu : time;
  signal icvz : std_logic_vector(0 downto 3);
  signal bnwx : std_logic_vector(4 to 2);
  signal wtuq : std_logic_vector(2 to 3);
  signal cqqxrcqbv : boolean_vector(3 to 2);
begin
  lwthhissla : entity work.dznylxw
    port map (hxtbhoisy => cqqxrcqbv, xslmhxtsj => wtuq);
  rrzydrglh : entity work.drol
    port map (cwylosu => bnwx, k => icvz, zyziwbuo => qmu);
  
  -- Single-driven assignments
  qmu <= 2#0_0.101# fs;
  wevbwhbq <= (others => '0');
  
  -- Multi-driven assignments
  wtuq <= ('W', '0');
end fdboyonkip;

entity vqvsy is
  port (yfnfziqfds : out real);
end vqvsy;

library ieee;
use ieee.std_logic_1164.all;

architecture atkuikr of vqvsy is
  signal ufkdxquja : std_logic_vector(2 to 3);
  signal zjgputry : boolean_vector(3 to 2);
  signal nzq : time;
  signal onpjjkpp : std_logic_vector(0 downto 3);
begin
  xyfs : entity work.drol
    port map (cwylosu => onpjjkpp, k => onpjjkpp, zyziwbuo => nzq);
  hkdaaxxo : entity work.dznylxw
    port map (hxtbhoisy => zjgputry, xslmhxtsj => ufkdxquja);
  
  -- Single-driven assignments
  yfnfziqfds <= 16#6.6_9#;
  nzq <= 140.30442 ns;
  
  -- Multi-driven assignments
  onpjjkpp <= (others => '0');
end atkuikr;



-- Seed after: 10664709819734574090,5472058987609252853
