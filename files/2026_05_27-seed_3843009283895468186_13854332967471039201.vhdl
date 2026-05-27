-- Seed: 3843009283895468186,13854332967471039201

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (mxufz : in time_vector(2 to 4); jdtjrqh : out character; kjayzqhem : buffer bit_vector(2 to 3); xytgo : out std_logic_vector(3 to 2));
end h;



architecture zfxxubzauk of h is
  
begin
  
end zfxxubzauk;

library ieee;
use ieee.std_logic_1164.all;

entity sy is
  port (ddpomkpg : linkage std_logic_vector(1 downto 0); iidpk : out real);
end sy;

library ieee;
use ieee.std_logic_1164.all;

architecture uowbrtdp of sy is
  signal mvxvbaclpo : bit_vector(2 to 3);
  signal a : character;
  signal qkiyqma : std_logic_vector(3 to 2);
  signal usetfax : bit_vector(2 to 3);
  signal ofpjfuu : character;
  signal ipa : time_vector(2 to 4);
  signal p : std_logic_vector(3 to 2);
  signal sbxko : bit_vector(2 to 3);
  signal mtga : character;
  signal lyutqhbgqi : time_vector(2 to 4);
begin
  zufjv : entity work.h
    port map (mxufz => lyutqhbgqi, jdtjrqh => mtga, kjayzqhem => sbxko, xytgo => p);
  kxbo : entity work.h
    port map (mxufz => ipa, jdtjrqh => ofpjfuu, kjayzqhem => usetfax, xytgo => qkiyqma);
  cxsejzdo : entity work.h
    port map (mxufz => ipa, jdtjrqh => a, kjayzqhem => mvxvbaclpo, xytgo => qkiyqma);
end uowbrtdp;

library ieee;
use ieee.std_logic_1164.all;

entity apdxhoyoqf is
  port (rpjtc : inout std_logic; ktpxmpq : inout real; sczkw : linkage std_logic; f : buffer std_logic);
end apdxhoyoqf;

library ieee;
use ieee.std_logic_1164.all;

architecture yb of apdxhoyoqf is
  signal lw : bit_vector(2 to 3);
  signal y : character;
  signal ldqca : std_logic_vector(3 to 2);
  signal fjlab : bit_vector(2 to 3);
  signal akfl : character;
  signal lv : time_vector(2 to 4);
begin
  n : entity work.h
    port map (mxufz => lv, jdtjrqh => akfl, kjayzqhem => fjlab, xytgo => ldqca);
  qmzrn : entity work.h
    port map (mxufz => lv, jdtjrqh => y, kjayzqhem => lw, xytgo => ldqca);
end yb;



-- Seed after: 216227623844995460,13854332967471039201
