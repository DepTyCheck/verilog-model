-- Seed: 11250797996252554260,11181851762153539145



entity grr is
  port (igposp : in bit_vector(0 to 3); v : linkage time; p : linkage severity_level);
end grr;



architecture ensdz of grr is
  
begin
  
end ensdz;



entity zgl is
  port ( tknxxz : inout integer_vector(2 downto 2)
  ; rf : in boolean_vector(2 to 2)
  ; elwmvuh : inout severity_level
  ; qoiczoq : inout bit_vector(1 downto 4)
  );
end zgl;



architecture ucl of zgl is
  signal hrzrwyunfx : severity_level;
  signal gtl : severity_level;
  signal xbzmrqqga : time;
  signal ayz : bit_vector(0 to 3);
begin
  jkq : entity work.grr
    port map (igposp => ayz, v => xbzmrqqga, p => gtl);
  u : entity work.grr
    port map (igposp => ayz, v => xbzmrqqga, p => hrzrwyunfx);
end ucl;

library ieee;
use ieee.std_logic_1164.all;

entity ekhujcoeom is
  port (ofzhdywioc : linkage std_logic; tu : out real_vector(2 downto 4));
end ekhujcoeom;



architecture b of ekhujcoeom is
  signal ortdmkor : bit_vector(1 downto 4);
  signal j : boolean_vector(2 to 2);
  signal gusjkgoe : integer_vector(2 downto 2);
  signal vcch : severity_level;
  signal tirffan : time;
  signal ixyklyfwer : severity_level;
  signal gjckamp : time;
  signal gd : bit_vector(0 to 3);
begin
  eetnfazec : entity work.grr
    port map (igposp => gd, v => gjckamp, p => ixyklyfwer);
  mhochoyil : entity work.grr
    port map (igposp => gd, v => tirffan, p => vcch);
  pp : entity work.zgl
    port map (tknxxz => gusjkgoe, rf => j, elwmvuh => vcch, qoiczoq => ortdmkor);
end b;

library ieee;
use ieee.std_logic_1164.all;

entity am is
  port (pg : buffer std_logic_vector(1 downto 2); vwvjw : buffer integer);
end am;

library ieee;
use ieee.std_logic_1164.all;

architecture ozc of am is
  signal pkvzaem : severity_level;
  signal jh : bit_vector(0 to 3);
  signal bekb : severity_level;
  signal b : time;
  signal yjy : bit_vector(0 to 3);
  signal i : bit_vector(1 downto 4);
  signal rumjiyee : severity_level;
  signal arnrsyhqj : boolean_vector(2 to 2);
  signal fyebvhc : integer_vector(2 downto 2);
  signal l : real_vector(2 downto 4);
  signal ypqabvi : std_logic;
begin
  ztzsynt : entity work.ekhujcoeom
    port map (ofzhdywioc => ypqabvi, tu => l);
  ymghn : entity work.zgl
    port map (tknxxz => fyebvhc, rf => arnrsyhqj, elwmvuh => rumjiyee, qoiczoq => i);
  yyi : entity work.grr
    port map (igposp => yjy, v => b, p => bekb);
  kg : entity work.grr
    port map (igposp => jh, v => b, p => pkvzaem);
end ozc;



-- Seed after: 3844737930939396821,11181851762153539145
