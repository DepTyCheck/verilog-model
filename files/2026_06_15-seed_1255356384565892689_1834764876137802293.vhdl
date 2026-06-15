-- Seed: 1255356384565892689,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity nptirpnn is
  port (hhyslmxchs : inout time; skzw : out real_vector(2 downto 4); sh : buffer std_logic);
end nptirpnn;

architecture oa of nptirpnn is
  
begin
  
end oa;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (lb : in real; ttc : inout std_logic_vector(1 to 0); wlqn : inout std_logic; whx : out integer);
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture wm of x is
  signal hyhlpq : std_logic;
  signal ealeuxfif : real_vector(2 downto 4);
  signal b : time;
begin
  amnpuzcv : entity work.nptirpnn
    port map (hhyslmxchs => b, skzw => ealeuxfif, sh => hyhlpq);
  
  -- Single-driven assignments
  whx <= 8#0_1_1_2#;
  
  -- Multi-driven assignments
  hyhlpq <= 'U';
  wlqn <= 'Z';
  wlqn <= 'X';
end wm;

entity f is
  port (ygvzmjnr : out integer; jent : inout real; zdo : inout time_vector(4 downto 0));
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture ykdwlqtjp of f is
  signal mxojyb : real_vector(2 downto 4);
  signal omtqho : time;
  signal mgzzh : real_vector(2 downto 4);
  signal qcrkmx : time;
  signal iqghow : std_logic;
  signal qgjsux : real_vector(2 downto 4);
  signal b : time;
begin
  ejm : entity work.nptirpnn
    port map (hhyslmxchs => b, skzw => qgjsux, sh => iqghow);
  eam : entity work.nptirpnn
    port map (hhyslmxchs => qcrkmx, skzw => mgzzh, sh => iqghow);
  ct : entity work.nptirpnn
    port map (hhyslmxchs => omtqho, skzw => mxojyb, sh => iqghow);
  
  -- Single-driven assignments
  jent <= 21.0444;
  zdo <= (3 sec, 2 sec, 04444.2024 us, 2#1_1_0_1_1# ms, 3.32042 ns);
  ygvzmjnr <= 3_4;
  
  -- Multi-driven assignments
  iqghow <= 'X';
  iqghow <= 'Z';
  iqghow <= 'X';
  iqghow <= '1';
end ykdwlqtjp;



-- Seed after: 14996579798515801845,1834764876137802293
