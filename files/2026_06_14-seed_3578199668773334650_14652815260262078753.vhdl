-- Seed: 3578199668773334650,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity ydw is
  port (jfevtyyjj : buffer integer; epeecfklwi : in integer_vector(2 downto 0); emyjiv : buffer std_logic);
end ydw;

architecture aytmednh of ydw is
  
begin
  -- Single-driven assignments
  jfevtyyjj <= 16#8_5_7#;
end aytmednh;

library ieee;
use ieee.std_logic_1164.all;

entity mpvazv is
  port (deeqers : inout std_logic_vector(4 to 2); bkiyqvdp : buffer real; yxsalagins : in real);
end mpvazv;

library ieee;
use ieee.std_logic_1164.all;

architecture opuuiba of mpvazv is
  signal lzjpl : integer;
  signal jchozmxv : std_logic;
  signal zoaf : integer;
  signal gakyenjnis : std_logic;
  signal q : integer_vector(2 downto 0);
  signal jnwofp : integer;
  signal ilkcrijq : std_logic;
  signal kvbtvdtsua : integer_vector(2 downto 0);
  signal pgv : integer;
begin
  syv : entity work.ydw
    port map (jfevtyyjj => pgv, epeecfklwi => kvbtvdtsua, emyjiv => ilkcrijq);
  muxzrzn : entity work.ydw
    port map (jfevtyyjj => jnwofp, epeecfklwi => q, emyjiv => gakyenjnis);
  womzqpylja : entity work.ydw
    port map (jfevtyyjj => zoaf, epeecfklwi => kvbtvdtsua, emyjiv => jchozmxv);
  iileguugc : entity work.ydw
    port map (jfevtyyjj => lzjpl, epeecfklwi => kvbtvdtsua, emyjiv => gakyenjnis);
  
  -- Single-driven assignments
  bkiyqvdp <= 1_4.4_0_1_4_3;
  kvbtvdtsua <= (044, 3_3, 8#5#);
  q <= (2#0_0_1_1_0#, 31, 13124);
  
  -- Multi-driven assignments
  jchozmxv <= 'U';
end opuuiba;



-- Seed after: 16255466806368259640,14652815260262078753
