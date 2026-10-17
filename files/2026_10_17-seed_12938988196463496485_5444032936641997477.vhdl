-- Seed: 12938988196463496485,5444032936641997477

entity cvkhsass is
  port (kxdzzcmg : inout integer_vector(3 downto 4); gyrmyfopcj : linkage bit_vector(2 downto 1); ijdovcidz : out integer; rnckye : in time);
end cvkhsass;

architecture mrebgemgm of cvkhsass is
  
begin
  -- Single-driven assignments
  kxdzzcmg <= (others => 0);
end mrebgemgm;

entity o is
  port (obd : buffer real; inc : in integer; jp : in time);
end o;

architecture zikpzswf of o is
  signal cizkh : integer;
  signal qtq : bit_vector(2 downto 1);
  signal gcb : integer_vector(3 downto 4);
  signal ef : integer;
  signal zovaomskk : bit_vector(2 downto 1);
  signal cuj : integer_vector(3 downto 4);
begin
  lfow : entity work.cvkhsass
    port map (kxdzzcmg => cuj, gyrmyfopcj => zovaomskk, ijdovcidz => ef, rnckye => jp);
  mewmmetc : entity work.cvkhsass
    port map (kxdzzcmg => gcb, gyrmyfopcj => qtq, ijdovcidz => cizkh, rnckye => jp);
end zikpzswf;

library ieee;
use ieee.std_logic_1164.all;

entity iunnjpeg is
  port (sebzbqsd : out std_logic_vector(0 downto 3));
end iunnjpeg;

architecture wqrwfci of iunnjpeg is
  signal ff : time;
  signal kqj : integer;
  signal wtx : bit_vector(2 downto 1);
  signal osryu : integer_vector(3 downto 4);
  signal tuz : time;
  signal yegij : integer;
  signal bctefy : bit_vector(2 downto 1);
  signal givcwlo : integer_vector(3 downto 4);
begin
  ewtaoyyiu : entity work.cvkhsass
    port map (kxdzzcmg => givcwlo, gyrmyfopcj => bctefy, ijdovcidz => yegij, rnckye => tuz);
  xvby : entity work.cvkhsass
    port map (kxdzzcmg => osryu, gyrmyfopcj => wtx, ijdovcidz => kqj, rnckye => ff);
  
  -- Single-driven assignments
  tuz <= 16#BAC# fs;
  ff <= ff;
end wqrwfci;



-- Seed after: 7865246474617270118,5444032936641997477
