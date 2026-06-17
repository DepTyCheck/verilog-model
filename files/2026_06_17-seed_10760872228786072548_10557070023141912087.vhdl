-- Seed: 10760872228786072548,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity scqc is
  port (ui : out std_logic; kqasuupgkn : buffer std_logic_vector(1 to 0); cgn : inout time_vector(2 to 1));
end scqc;

architecture exttf of scqc is
  
begin
  -- Single-driven assignments
  cgn <= (others => 0 ns);
end exttf;

entity uxp is
  port (rxrhlgkbj : in integer_vector(1 downto 0); cputuyhx : in integer);
end uxp;

library ieee;
use ieee.std_logic_1164.all;

architecture r of uxp is
  signal l : time_vector(2 to 1);
  signal qqopqp : std_logic;
  signal kgxgd : time_vector(2 to 1);
  signal pvrxu : std_logic_vector(1 to 0);
  signal wonjfpcpij : std_logic;
  signal nzjuv : time_vector(2 to 1);
  signal octwo : time_vector(2 to 1);
  signal tkbbkpvgly : std_logic_vector(1 to 0);
  signal bmxwtnxuh : std_logic;
begin
  admhhgxel : entity work.scqc
    port map (ui => bmxwtnxuh, kqasuupgkn => tkbbkpvgly, cgn => octwo);
  kjvn : entity work.scqc
    port map (ui => bmxwtnxuh, kqasuupgkn => tkbbkpvgly, cgn => nzjuv);
  cs : entity work.scqc
    port map (ui => wonjfpcpij, kqasuupgkn => pvrxu, cgn => kgxgd);
  sgmyjzimbf : entity work.scqc
    port map (ui => qqopqp, kqasuupgkn => tkbbkpvgly, cgn => l);
  
  -- Multi-driven assignments
  pvrxu <= "";
  wonjfpcpij <= 'W';
  bmxwtnxuh <= 'H';
  tkbbkpvgly <= (others => '0');
end r;

entity yjwvc is
  port (se : in severity_level; deffz : out time; rsgqiqlz : linkage time_vector(2 downto 4));
end yjwvc;

library ieee;
use ieee.std_logic_1164.all;

architecture v of yjwvc is
  signal o : time_vector(2 to 1);
  signal a : std_logic;
  signal uudrgawdy : time_vector(2 to 1);
  signal vkeermq : std_logic_vector(1 to 0);
  signal asvu : time_vector(2 to 1);
  signal koykhgxm : std_logic_vector(1 to 0);
  signal hiklzln : std_logic;
begin
  smmqtaja : entity work.scqc
    port map (ui => hiklzln, kqasuupgkn => koykhgxm, cgn => asvu);
  ygbokz : entity work.scqc
    port map (ui => hiklzln, kqasuupgkn => vkeermq, cgn => uudrgawdy);
  boclipwv : entity work.scqc
    port map (ui => a, kqasuupgkn => vkeermq, cgn => o);
end v;



-- Seed after: 11249882674872416338,10557070023141912087
