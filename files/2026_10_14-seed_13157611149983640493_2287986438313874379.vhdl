-- Seed: 13157611149983640493,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity xba is
  port (vnnrpsw : in time; gydchsasf : in std_logic);
end xba;

architecture kmm of xba is
  
begin
  
end kmm;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (cugcvk : out std_logic; aqysfqfman : out integer; erograx : linkage integer; mczjyltf : out time);
end s;

library ieee;
use ieee.std_logic_1164.all;

architecture yxv of s is
  signal uqrjljkr : time;
  signal xcgkhsvtdj : time;
  signal w : std_logic;
  signal vfd : time;
begin
  emjaiuxjt : entity work.xba
    port map (vnnrpsw => vfd, gydchsasf => w);
  nbrtm : entity work.xba
    port map (vnnrpsw => xcgkhsvtdj, gydchsasf => cugcvk);
  nrryudgsgm : entity work.xba
    port map (vnnrpsw => uqrjljkr, gydchsasf => cugcvk);
  
  -- Single-driven assignments
  aqysfqfman <= 4;
end yxv;

entity uifa is
  port (qpkk : out time; bvcbwskuzw : linkage integer);
end uifa;

library ieee;
use ieee.std_logic_1164.all;

architecture bzpm of uifa is
  signal fygmubxqpa : time;
  signal wt : integer;
  signal ly : std_logic;
  signal tnpsrw : time;
  signal j : std_logic;
begin
  vxpr : entity work.xba
    port map (vnnrpsw => qpkk, gydchsasf => j);
  wkkmqzv : entity work.xba
    port map (vnnrpsw => tnpsrw, gydchsasf => j);
  yo : entity work.xba
    port map (vnnrpsw => tnpsrw, gydchsasf => j);
  avotckmrd : entity work.s
    port map (cugcvk => ly, aqysfqfman => wt, erograx => bvcbwskuzw, mczjyltf => fygmubxqpa);
end bzpm;

library ieee;
use ieee.std_logic_1164.all;

entity abtdgvxej is
  port (hklzslrxt : out std_logic_vector(2 to 0); haszjrzpic : in integer; jleqovhqt : linkage bit_vector(4 downto 1); qpmfkwprd : linkage std_logic);
end abtdgvxej;

library ieee;
use ieee.std_logic_1164.all;

architecture idqshfw of abtdgvxej is
  signal canu : time;
  signal gmm : time;
  signal d : integer;
  signal hkflkel : integer;
  signal ljxqowskh : std_logic;
  signal rrhsg : integer;
  signal jw : time;
begin
  awddxcvd : entity work.uifa
    port map (qpkk => jw, bvcbwskuzw => rrhsg);
  rxpdivg : entity work.s
    port map (cugcvk => ljxqowskh, aqysfqfman => hkflkel, erograx => d, mczjyltf => gmm);
  lf : entity work.xba
    port map (vnnrpsw => canu, gydchsasf => ljxqowskh);
  
  -- Single-driven assignments
  canu <= jw;
  
  -- Multi-driven assignments
  ljxqowskh <= 'W';
  hklzslrxt <= hklzslrxt;
end idqshfw;



-- Seed after: 10755131086271392501,2287986438313874379
