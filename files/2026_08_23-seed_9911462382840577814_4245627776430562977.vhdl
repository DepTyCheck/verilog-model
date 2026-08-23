-- Seed: 9911462382840577814,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity vr is
  port (ycx : inout std_logic; cmsfmpy : in boolean);
end vr;

architecture wbo of vr is
  
begin
  
end wbo;

library ieee;
use ieee.std_logic_1164.all;

entity sls is
  port (kxpcwiqguv : inout std_logic; wamkgnazk : in std_logic; uiapvlpa : out character; xbc : in integer);
end sls;

architecture btgkvony of sls is
  signal iaswolkl : boolean;
  signal wmljtbv : boolean;
  signal rfoxlwc : boolean;
begin
  bvbz : entity work.vr
    port map (ycx => kxpcwiqguv, cmsfmpy => rfoxlwc);
  ycyic : entity work.vr
    port map (ycx => kxpcwiqguv, cmsfmpy => wmljtbv);
  ocvtyuza : entity work.vr
    port map (ycx => kxpcwiqguv, cmsfmpy => rfoxlwc);
  rl : entity work.vr
    port map (ycx => kxpcwiqguv, cmsfmpy => iaswolkl);
  
  -- Multi-driven assignments
  kxpcwiqguv <= wamkgnazk;
end btgkvony;

library ieee;
use ieee.std_logic_1164.all;

entity lthlhe is
  port (nl : out integer; wtlpa : in std_logic_vector(1 downto 1));
end lthlhe;

library ieee;
use ieee.std_logic_1164.all;

architecture bloq of lthlhe is
  signal zbl : std_logic;
  signal bwx : boolean;
  signal wyfwiz : std_logic;
  signal mhytqeu : integer;
  signal tm : character;
  signal tg : std_logic;
  signal cqle : integer;
  signal hfcuic : character;
  signal truxrvw : std_logic;
begin
  wfzoyxra : entity work.sls
    port map (kxpcwiqguv => truxrvw, wamkgnazk => truxrvw, uiapvlpa => hfcuic, xbc => cqle);
  sedx : entity work.sls
    port map (kxpcwiqguv => tg, wamkgnazk => tg, uiapvlpa => tm, xbc => mhytqeu);
  upmoi : entity work.vr
    port map (ycx => wyfwiz, cmsfmpy => bwx);
  idzggiyi : entity work.vr
    port map (ycx => zbl, cmsfmpy => bwx);
end bloq;



-- Seed after: 13787826641685620357,4245627776430562977
