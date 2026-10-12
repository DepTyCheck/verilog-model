-- Seed: 14776232845537825005,3426943171337919827

entity xf is
  port (qsfsud : linkage real; brt : linkage bit; n : buffer character);
end xf;

architecture xgsa of xf is
  
begin
  -- Single-driven assignments
  n <= 'w';
end xgsa;

entity latx is
  port (jgropinqvz : out string(4 to 4); wq : inout time; i : out time);
end latx;

architecture ubifhbitzy of latx is
  
begin
  -- Single-driven assignments
  i <= 4 ms;
  jgropinqvz <= jgropinqvz;
  wq <= i;
end ubifhbitzy;

library ieee;
use ieee.std_logic_1164.all;

entity dvqnp is
  port (jlluznugsr : inout time; fofkqiw : buffer time; jasqomol : buffer string(2 downto 3); ul : buffer std_logic_vector(4 downto 4));
end dvqnp;

architecture qufurynhiv of dvqnp is
  signal amyhogotzy : character;
  signal tr : bit;
  signal erznswtil : real;
  signal lhqvskp : string(4 to 4);
  signal gqiqax : time;
  signal p : time;
  signal a : string(4 to 4);
begin
  upvinmkgqn : entity work.latx
    port map (jgropinqvz => a, wq => p, i => gqiqax);
  qmbfz : entity work.latx
    port map (jgropinqvz => lhqvskp, wq => fofkqiw, i => jlluznugsr);
  clwe : entity work.xf
    port map (qsfsud => erznswtil, brt => tr, n => amyhogotzy);
  
  -- Single-driven assignments
  jasqomol <= jasqomol;
end qufurynhiv;



-- Seed after: 11135420458947710215,3426943171337919827
