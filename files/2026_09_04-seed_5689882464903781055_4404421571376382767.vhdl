-- Seed: 5689882464903781055,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity kflazqd is
  port (d : inout time; hwgdudn : linkage std_logic_vector(2 to 0); paxvbe : out real);
end kflazqd;

architecture ryyurxwp of kflazqd is
  
begin
  -- Single-driven assignments
  paxvbe <= 16#863F.4#;
  d <= 1_0_3_0_3.0_3_2 ms;
end ryyurxwp;

entity pbrm is
  port (jyijsekxu : in character);
end pbrm;

library ieee;
use ieee.std_logic_1164.all;

architecture uhendqkoiu of pbrm is
  signal zqzkzioe : real;
  signal zrioafuong : std_logic_vector(2 to 0);
  signal yzzfwgp : time;
  signal ilqfnqx : real;
  signal m : std_logic_vector(2 to 0);
  signal llc : time;
begin
  nzvsao : entity work.kflazqd
    port map (d => llc, hwgdudn => m, paxvbe => ilqfnqx);
  ehledhg : entity work.kflazqd
    port map (d => yzzfwgp, hwgdudn => zrioafuong, paxvbe => zqzkzioe);
  
  -- Multi-driven assignments
  m <= m;
  m <= zrioafuong;
  m <= m;
end uhendqkoiu;

entity ipv is
  port (ol : out time);
end ipv;

library ieee;
use ieee.std_logic_1164.all;

architecture sofxplkzo of ipv is
  signal fhrxqzpxmh : real;
  signal dh : real;
  signal bladvrd : time;
  signal y : real;
  signal hlastit : std_logic_vector(2 to 0);
  signal lhf : time;
begin
  gb : entity work.kflazqd
    port map (d => lhf, hwgdudn => hlastit, paxvbe => y);
  vhwddwiyic : entity work.kflazqd
    port map (d => bladvrd, hwgdudn => hlastit, paxvbe => dh);
  eag : entity work.kflazqd
    port map (d => ol, hwgdudn => hlastit, paxvbe => fhrxqzpxmh);
  
  -- Multi-driven assignments
  hlastit <= (others => '0');
  hlastit <= (others => '0');
end sofxplkzo;

entity fx is
  port (xq : in boolean; xpxbuixk : in real_vector(4 downto 0); fqekr : linkage integer);
end fx;

library ieee;
use ieee.std_logic_1164.all;

architecture bz of fx is
  signal g : real;
  signal glmukx : time;
  signal wd : character;
  signal qyszlf : real;
  signal rrywmkqfh : std_logic_vector(2 to 0);
  signal tgkjdcwxwz : time;
begin
  q : entity work.kflazqd
    port map (d => tgkjdcwxwz, hwgdudn => rrywmkqfh, paxvbe => qyszlf);
  w : entity work.pbrm
    port map (jyijsekxu => wd);
  prujt : entity work.kflazqd
    port map (d => glmukx, hwgdudn => rrywmkqfh, paxvbe => g);
  nwsgzyog : entity work.pbrm
    port map (jyijsekxu => wd);
end bz;



-- Seed after: 7475479031313136153,4404421571376382767
