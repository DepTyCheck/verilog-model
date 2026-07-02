-- Seed: 11857588647379968379,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity zohvg is
  port (voyi : buffer std_logic; zyxme : inout integer; tjpgdksx : linkage bit_vector(3 downto 2));
end zohvg;

architecture zvanm of zohvg is
  
begin
  -- Multi-driven assignments
  voyi <= 'W';
  voyi <= 'X';
end zvanm;

entity tsnsclt is
  port (dvo : out bit_vector(1 downto 0); sv : out real_vector(3 downto 3); h : out time; kkbcudvc : inout time_vector(3 to 2));
end tsnsclt;

library ieee;
use ieee.std_logic_1164.all;

architecture cxbsj of tsnsclt is
  signal elaejfjsjs : integer;
  signal brfait : bit_vector(3 downto 2);
  signal bqvwjy : integer;
  signal eswjil : std_logic;
begin
  zfek : entity work.zohvg
    port map (voyi => eswjil, zyxme => bqvwjy, tjpgdksx => brfait);
  ypjtd : entity work.zohvg
    port map (voyi => eswjil, zyxme => elaejfjsjs, tjpgdksx => dvo);
  
  -- Single-driven assignments
  kkbcudvc <= (others => 0 ns);
  sv <= (others => 2#1_0_1_1_1.10000#);
  h <= 8#6_5_1_0_5.752# us;
  
  -- Multi-driven assignments
  eswjil <= 'U';
  eswjil <= 'L';
  eswjil <= '1';
  eswjil <= 'W';
end cxbsj;



-- Seed after: 1401082442236047174,13694093582652240945
