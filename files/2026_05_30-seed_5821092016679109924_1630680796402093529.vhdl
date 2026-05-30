-- Seed: 5821092016679109924,1630680796402093529

library ieee;
use ieee.std_logic_1164.all;

entity yga is
  port (e : inout integer_vector(2 downto 0); kxxfbfjy : inout std_logic_vector(1 to 4); u : in real; imyf : linkage bit_vector(4 to 1));
end yga;



architecture bnhxb of yga is
  
begin
  
end bnhxb;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (sk : inout std_logic_vector(2 downto 0));
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture m of p is
  signal d : bit_vector(4 to 1);
  signal b : real;
  signal eddjlxjtd : std_logic_vector(1 to 4);
  signal mwbnoysi : integer_vector(2 downto 0);
begin
  axnsu : entity work.yga
    port map (e => mwbnoysi, kxxfbfjy => eddjlxjtd, u => b, imyf => d);
end m;



entity absac is
  port (rn : in integer);
end absac;

library ieee;
use ieee.std_logic_1164.all;

architecture rfvyq of absac is
  signal skpoqdvx : real;
  signal my : std_logic_vector(1 to 4);
  signal ixniumn : integer_vector(2 downto 0);
  signal c : bit_vector(4 to 1);
  signal cublww : real;
  signal iaujtpb : std_logic_vector(1 to 4);
  signal bqahteavkm : integer_vector(2 downto 0);
begin
  lmiibvq : entity work.yga
    port map (e => bqahteavkm, kxxfbfjy => iaujtpb, u => cublww, imyf => c);
  pkmnxa : entity work.yga
    port map (e => ixniumn, kxxfbfjy => my, u => skpoqdvx, imyf => c);
end rfvyq;



-- Seed after: 1400827811863024263,1630680796402093529
