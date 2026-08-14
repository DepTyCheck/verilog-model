-- Seed: 8974878570280558400,8437298063418820479

entity xpyh is
  port (rbwfnpnj : in time_vector(3 downto 0));
end xpyh;

architecture wlfuprl of xpyh is
  
begin
  
end wlfuprl;

library ieee;
use ieee.std_logic_1164.all;

entity mhapd is
  port (savztk : out std_logic_vector(3 to 0));
end mhapd;

architecture bxmdt of mhapd is
  
begin
  
end bxmdt;

entity vfl is
  port (teet : buffer time_vector(0 downto 1));
end vfl;

architecture dabw of vfl is
  
begin
  -- Single-driven assignments
  teet <= (others => 0 ns);
end dabw;

entity oentehj is
  port (dykfhif : buffer time);
end oentehj;

architecture hf of oentehj is
  signal vooub : time_vector(3 downto 0);
  signal vdvzxibeb : time_vector(3 downto 0);
  signal cmf : time_vector(3 downto 0);
  signal nouuxv : time_vector(0 downto 1);
begin
  jzyjdfd : entity work.vfl
    port map (teet => nouuxv);
  t : entity work.xpyh
    port map (rbwfnpnj => cmf);
  szhbyros : entity work.xpyh
    port map (rbwfnpnj => vdvzxibeb);
  phmalis : entity work.xpyh
    port map (rbwfnpnj => vooub);
  
  -- Single-driven assignments
  cmf <= (16#3.CAEA2# ns, 3 sec, 2#01010.1# ns, 16#B_6_6.C_A_0_E# ps);
  vooub <= vdvzxibeb;
  vdvzxibeb <= cmf;
  dykfhif <= dykfhif;
end hf;



-- Seed after: 14673954126975617012,8437298063418820479
