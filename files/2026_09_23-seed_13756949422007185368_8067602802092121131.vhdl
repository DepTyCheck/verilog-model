-- Seed: 13756949422007185368,8067602802092121131

entity rri is
  port (cf : buffer boolean; ag : out real; xxdsnoir : in real_vector(4 downto 1); hszrdloy : in real);
end rri;

architecture ckggm of rri is
  
begin
  -- Single-driven assignments
  ag <= hszrdloy;
end ckggm;

library ieee;
use ieee.std_logic_1164.all;

entity sjmpsumwb is
  port (plcyaruyen : in std_logic; gp : in std_logic_vector(1 downto 3); v : out time);
end sjmpsumwb;

architecture syeiphvifg of sjmpsumwb is
  
begin
  -- Single-driven assignments
  v <= 4023 ns;
end syeiphvifg;

entity fuvj is
  port (v : in integer_vector(2 downto 4); mncjcce : out time_vector(2 downto 3); zaayhoqon : buffer bit);
end fuvj;

library ieee;
use ieee.std_logic_1164.all;

architecture mn of fuvj is
  signal rs : real_vector(4 downto 1);
  signal i : boolean;
  signal jlk : time;
  signal fbqmvgbag : real;
  signal xihlrlo : real_vector(4 downto 1);
  signal anclpwer : real;
  signal ya : boolean;
  signal mkbz : time;
  signal sstwhkre : std_logic_vector(1 downto 3);
  signal zn : std_logic;
begin
  tfbjewar : entity work.sjmpsumwb
    port map (plcyaruyen => zn, gp => sstwhkre, v => mkbz);
  zrrwvlkys : entity work.rri
    port map (cf => ya, ag => anclpwer, xxdsnoir => xihlrlo, hszrdloy => fbqmvgbag);
  jvms : entity work.sjmpsumwb
    port map (plcyaruyen => zn, gp => sstwhkre, v => jlk);
  rrkye : entity work.rri
    port map (cf => i, ag => fbqmvgbag, xxdsnoir => rs, hszrdloy => fbqmvgbag);
  
  -- Single-driven assignments
  mncjcce <= (others => 0 ns);
  rs <= xihlrlo;
  zaayhoqon <= '0';
  xihlrlo <= xihlrlo;
end mn;



-- Seed after: 13158074213674832696,8067602802092121131
