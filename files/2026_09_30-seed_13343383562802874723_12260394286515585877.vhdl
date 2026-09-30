-- Seed: 13343383562802874723,12260394286515585877

entity wju is
  port (nqbxa : buffer character; rrgkwm : buffer character; isxorry : in time);
end wju;

architecture aqgwgc of wju is
  
begin
  -- Single-driven assignments
  nqbxa <= 'c';
  rrgkwm <= 'w';
end aqgwgc;

entity gipc is
  port (q : inout real);
end gipc;

architecture ahyjplzpzv of gipc is
  
begin
  -- Single-driven assignments
  q <= q;
end ahyjplzpzv;

library ieee;
use ieee.std_logic_1164.all;

entity aoy is
  port (kf : inout bit_vector(1 to 3); llcrfmclq : out boolean_vector(4 downto 0); qizorpsdls : out integer; hvzeutio : inout std_logic);
end aoy;

architecture cj of aoy is
  signal s : time;
  signal ylaefuu : character;
  signal okhbk : character;
  signal yzhlkrbcb : time;
  signal rlos : character;
  signal pmlnho : character;
  signal hnbfu : time;
  signal aphiixipko : character;
  signal uqkdjr : character;
  signal hqwsvncfh : real;
begin
  ewntdu : entity work.gipc
    port map (q => hqwsvncfh);
  rfijjulv : entity work.wju
    port map (nqbxa => uqkdjr, rrgkwm => aphiixipko, isxorry => hnbfu);
  fq : entity work.wju
    port map (nqbxa => pmlnho, rrgkwm => rlos, isxorry => yzhlkrbcb);
  fuxy : entity work.wju
    port map (nqbxa => okhbk, rrgkwm => ylaefuu, isxorry => s);
  
  -- Multi-driven assignments
  hvzeutio <= hvzeutio;
  hvzeutio <= '1';
  hvzeutio <= 'W';
  hvzeutio <= 'U';
end cj;



-- Seed after: 10397221074797791396,12260394286515585877
