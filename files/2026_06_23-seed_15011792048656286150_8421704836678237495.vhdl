-- Seed: 15011792048656286150,8421704836678237495

entity qtkfrsfbw is
  port (edzn : buffer bit_vector(0 downto 3); yp : inout time_vector(4 downto 2));
end qtkfrsfbw;

architecture fhimlxjlfa of qtkfrsfbw is
  
begin
  -- Single-driven assignments
  edzn <= (others => '0');
  yp <= (2#0_1_0_0_0.1_1_1# ps, 2#0_1# ms, 2 sec);
end fhimlxjlfa;

library ieee;
use ieee.std_logic_1164.all;

entity rksrl is
  port (ceqqd : linkage time; q : out std_logic; xy : inout real; hceqxkts : linkage std_logic_vector(4 downto 1));
end rksrl;

architecture moasunvwdw of rksrl is
  signal bbgl : time_vector(4 downto 2);
  signal yaourpptre : bit_vector(0 downto 3);
  signal rlj : time_vector(4 downto 2);
  signal oghdbxl : bit_vector(0 downto 3);
begin
  p : entity work.qtkfrsfbw
    port map (edzn => oghdbxl, yp => rlj);
  ctnm : entity work.qtkfrsfbw
    port map (edzn => yaourpptre, yp => bbgl);
  
  -- Single-driven assignments
  xy <= 4122.23424;
  
  -- Multi-driven assignments
  q <= '-';
  q <= 'Z';
  q <= 'U';
  q <= 'X';
end moasunvwdw;

entity bp is
  port (rxruc : linkage real; gpejgdxc : inout real);
end bp;

architecture ycsrgvqt of bp is
  
begin
  -- Single-driven assignments
  gpejgdxc <= 2.0_3;
end ycsrgvqt;



-- Seed after: 2244679436014339082,8421704836678237495
