-- Seed: 9883688975335243161,16344534354979482531

library ieee;
use ieee.std_logic_1164.all;

entity savahsuf is
  port (qnwtnxmdh : buffer time; hgdxch : in std_logic; qddpzvh : in std_logic; sjpwyp : inout real);
end savahsuf;



architecture xg of savahsuf is
  
begin
  
end xg;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (xxf : linkage std_logic; kmnhxjn : inout std_logic);
end t;



architecture adumrm of t is
  
begin
  
end adumrm;



entity zqcdm is
  port (hx : in real; edu : in time; nllzsp : out boolean);
end zqcdm;

library ieee;
use ieee.std_logic_1164.all;

architecture luqhyiikx of zqcdm is
  signal hmuxh : real;
  signal yvhzvh : std_logic;
  signal kv : std_logic;
  signal zgsown : time;
begin
  uwxohdc : entity work.savahsuf
    port map (qnwtnxmdh => zgsown, hgdxch => kv, qddpzvh => yvhzvh, sjpwyp => hmuxh);
end luqhyiikx;



entity kxuk is
  port (zxcom : buffer real);
end kxuk;

library ieee;
use ieee.std_logic_1164.all;

architecture xw of kxuk is
  signal achyedhs : time;
  signal zluhk : std_logic;
  signal wkapfeixg : std_logic;
  signal pbritk : boolean;
  signal tiwyesk : time;
begin
  zcevsuv : entity work.zqcdm
    port map (hx => zxcom, edu => tiwyesk, nllzsp => pbritk);
  aybnyf : entity work.t
    port map (xxf => wkapfeixg, kmnhxjn => zluhk);
  abwftahanq : entity work.t
    port map (xxf => wkapfeixg, kmnhxjn => wkapfeixg);
  rzd : entity work.savahsuf
    port map (qnwtnxmdh => achyedhs, hgdxch => wkapfeixg, qddpzvh => zluhk, sjpwyp => zxcom);
end xw;



-- Seed after: 10257410830369181995,16344534354979482531
