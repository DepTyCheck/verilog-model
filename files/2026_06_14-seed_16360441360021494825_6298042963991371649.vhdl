-- Seed: 16360441360021494825,6298042963991371649

library ieee;
use ieee.std_logic_1164.all;

entity qgegoavao is
  port (ywn : out time; qewxiinjk : buffer std_logic_vector(4 downto 2); b : linkage bit_vector(2 to 3); z : buffer character);
end qgegoavao;



architecture hvhaieza of qgegoavao is
  
begin
  
end hvhaieza;



entity c is
  port (mhkubgdk : in boolean_vector(4 downto 3); mndfw : linkage time; jydqx : out integer; xqqzln : linkage boolean);
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture vncd of c is
  signal ybzbztwcu : character;
  signal ikacll : bit_vector(2 to 3);
  signal crwk : time;
  signal mcwoknyy : character;
  signal cvuo : bit_vector(2 to 3);
  signal ap : std_logic_vector(4 downto 2);
  signal m : time;
begin
  k : entity work.qgegoavao
    port map (ywn => m, qewxiinjk => ap, b => cvuo, z => mcwoknyy);
  rjqekchytq : entity work.qgegoavao
    port map (ywn => crwk, qewxiinjk => ap, b => ikacll, z => ybzbztwcu);
end vncd;

library ieee;
use ieee.std_logic_1164.all;

entity vzgeqba is
  port (dmhg : linkage character; fotp : buffer boolean_vector(4 downto 1); kbo : out std_logic_vector(2 to 3));
end vzgeqba;

library ieee;
use ieee.std_logic_1164.all;

architecture r of vzgeqba is
  signal x : character;
  signal rppum : bit_vector(2 to 3);
  signal uywlkkif : std_logic_vector(4 downto 2);
  signal jeh : time;
  signal sbiwj : boolean;
  signal vchklkg : integer;
  signal ehwdzijtul : time;
  signal vansuj : boolean_vector(4 downto 3);
begin
  idfxr : entity work.c
    port map (mhkubgdk => vansuj, mndfw => ehwdzijtul, jydqx => vchklkg, xqqzln => sbiwj);
  chulhovg : entity work.qgegoavao
    port map (ywn => jeh, qewxiinjk => uywlkkif, b => rppum, z => x);
end r;



-- Seed after: 14599434840392567299,6298042963991371649
