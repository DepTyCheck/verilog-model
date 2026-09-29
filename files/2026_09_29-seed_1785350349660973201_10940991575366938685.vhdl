-- Seed: 1785350349660973201,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity aihd is
  port (a : inout time; yxckorqst : in time; ddnqmldenz : out std_logic_vector(0 downto 4); xvfelgjq : in std_logic);
end aihd;

architecture ddzljrxag of aihd is
  
begin
  -- Single-driven assignments
  a <= 33 ms;
  
  -- Multi-driven assignments
  ddnqmldenz <= ddnqmldenz;
end ddzljrxag;

entity jgrtuj is
  port (umnr : out integer);
end jgrtuj;

library ieee;
use ieee.std_logic_1164.all;

architecture ayw of jgrtuj is
  signal fcivoitx : std_logic;
  signal o : std_logic_vector(0 downto 4);
  signal plyfpeu : time;
  signal v : std_logic_vector(0 downto 4);
  signal gow : std_logic_vector(0 downto 4);
  signal qjdomo : time;
  signal fxp : std_logic;
  signal yfjhqkub : std_logic_vector(0 downto 4);
  signal bjsjky : time;
  signal a : time;
begin
  mpnbxfpu : entity work.aihd
    port map (a => a, yxckorqst => bjsjky, ddnqmldenz => yfjhqkub, xvfelgjq => fxp);
  iqiqjj : entity work.aihd
    port map (a => bjsjky, yxckorqst => qjdomo, ddnqmldenz => gow, xvfelgjq => fxp);
  dzkpme : entity work.aihd
    port map (a => qjdomo, yxckorqst => bjsjky, ddnqmldenz => v, xvfelgjq => fxp);
  zlbhusae : entity work.aihd
    port map (a => plyfpeu, yxckorqst => plyfpeu, ddnqmldenz => o, xvfelgjq => fcivoitx);
  
  -- Single-driven assignments
  umnr <= 2#1#;
  
  -- Multi-driven assignments
  v <= "";
  v <= yfjhqkub;
end ayw;



-- Seed after: 11605525835240149134,10940991575366938685
