-- Seed: 12313137020465153003,7311216359267151659

entity iwrautez is
  port (mqcqk : linkage real; hvchyjefbo : out bit_vector(4 downto 3); wamuvfdwqw : out time; wyrwkkth : linkage character);
end iwrautez;

architecture cl of iwrautez is
  
begin
  -- Single-driven assignments
  wamuvfdwqw <= wamuvfdwqw;
  hvchyjefbo <= hvchyjefbo;
end cl;

entity ywwsax is
  port (nqhttdfj : in real; qmdyfpwep : in time; liljhnlsfm : inout time);
end ywwsax;

architecture lqobtckmfi of ywwsax is
  signal uwo : character;
  signal cwxgrqvwkk : bit_vector(4 downto 3);
  signal fqhoyexsrb : real;
begin
  fygl : entity work.iwrautez
    port map (mqcqk => fqhoyexsrb, hvchyjefbo => cwxgrqvwkk, wamuvfdwqw => liljhnlsfm, wyrwkkth => uwo);
end lqobtckmfi;

library ieee;
use ieee.std_logic_1164.all;

entity ynkbcx is
  port (fkvybrjh : buffer std_logic_vector(4 to 2); ua : linkage integer);
end ynkbcx;

architecture ootgomqmt of ynkbcx is
  signal ep : time;
  signal hmkd : character;
  signal ifpnowrsu : time;
  signal zcgem : bit_vector(4 downto 3);
  signal dwipq : real;
begin
  rlnffpdes : entity work.iwrautez
    port map (mqcqk => dwipq, hvchyjefbo => zcgem, wamuvfdwqw => ifpnowrsu, wyrwkkth => hmkd);
  mcjqfq : entity work.ywwsax
    port map (nqhttdfj => dwipq, qmdyfpwep => ep, liljhnlsfm => ep);
  
  -- Multi-driven assignments
  fkvybrjh <= (others => '0');
end ootgomqmt;



-- Seed after: 7974933990138401370,7311216359267151659
