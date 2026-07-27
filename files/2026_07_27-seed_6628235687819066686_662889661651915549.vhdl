-- Seed: 6628235687819066686,662889661651915549

entity kjmehk is
  port (wmc : linkage bit_vector(3 to 2));
end kjmehk;

architecture hfj of kjmehk is
  
begin
  
end hfj;

entity x is
  port (rkpjnii : in integer);
end x;

architecture oa of x is
  signal b : bit_vector(3 to 2);
  signal esrjjev : bit_vector(3 to 2);
begin
  ifserctkui : entity work.kjmehk
    port map (wmc => esrjjev);
  y : entity work.kjmehk
    port map (wmc => b);
end oa;

library ieee;
use ieee.std_logic_1164.all;

entity om is
  port (wxcdunlfr : inout bit; c : buffer std_logic_vector(3 to 3));
end om;

architecture qxeu of om is
  signal jkzeitorqx : bit_vector(3 to 2);
begin
  gxmx : entity work.kjmehk
    port map (wmc => jkzeitorqx);
  
  -- Single-driven assignments
  wxcdunlfr <= wxcdunlfr;
  
  -- Multi-driven assignments
  c <= "L";
  c <= "L";
  c <= c;
  c <= "-";
end qxeu;

entity zji is
  port (zkaly : linkage real_vector(2 downto 2); nfhsfjuf : linkage bit);
end zji;

library ieee;
use ieee.std_logic_1164.all;

architecture tkrqxyeon of zji is
  signal ahiqa : std_logic_vector(3 to 3);
  signal zwcpk : bit;
  signal wpqpuue : bit_vector(3 to 2);
  signal bkd : integer;
  signal i : bit_vector(3 to 2);
begin
  ymriesvcp : entity work.kjmehk
    port map (wmc => i);
  a : entity work.x
    port map (rkpjnii => bkd);
  drovjs : entity work.kjmehk
    port map (wmc => wpqpuue);
  eain : entity work.om
    port map (wxcdunlfr => zwcpk, c => ahiqa);
  
  -- Single-driven assignments
  bkd <= bkd;
  
  -- Multi-driven assignments
  ahiqa <= (others => 'H');
  ahiqa <= ahiqa;
  ahiqa <= "H";
end tkrqxyeon;



-- Seed after: 14601781443499185501,662889661651915549
