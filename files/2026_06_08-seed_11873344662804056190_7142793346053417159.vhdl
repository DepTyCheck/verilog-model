-- Seed: 11873344662804056190,7142793346053417159

library ieee;
use ieee.std_logic_1164.all;

entity aipuey is
  port (godqkzdibc : linkage real; icft : inout integer; biovwtu : linkage time; btndhxsq : out std_logic_vector(2 downto 1));
end aipuey;



architecture nd of aipuey is
  
begin
  
end nd;

library ieee;
use ieee.std_logic_1164.all;

entity fzgqmigtd is
  port (mp : in time; ujzqzip : linkage integer; chou : out integer; anqf : inout std_logic);
end fzgqmigtd;

library ieee;
use ieee.std_logic_1164.all;

architecture ikeqox of fzgqmigtd is
  signal wy : real;
  signal miywjda : time;
  signal xgqcyouhfe : integer;
  signal jwncixgyq : std_logic_vector(2 downto 1);
  signal mom : time;
  signal tmjct : integer;
  signal doocrcomm : real;
  signal an : std_logic_vector(2 downto 1);
  signal hxfnaask : time;
  signal fawgouxmtc : integer;
  signal oyn : real;
begin
  ocey : entity work.aipuey
    port map (godqkzdibc => oyn, icft => fawgouxmtc, biovwtu => hxfnaask, btndhxsq => an);
  jnic : entity work.aipuey
    port map (godqkzdibc => doocrcomm, icft => tmjct, biovwtu => mom, btndhxsq => jwncixgyq);
  i : entity work.aipuey
    port map (godqkzdibc => oyn, icft => xgqcyouhfe, biovwtu => miywjda, btndhxsq => jwncixgyq);
  pzvk : entity work.aipuey
    port map (godqkzdibc => wy, icft => chou, biovwtu => miywjda, btndhxsq => jwncixgyq);
end ikeqox;



entity ylatwhk is
  port (mhtnnxjs : out boolean; pv : buffer time);
end ylatwhk;

library ieee;
use ieee.std_logic_1164.all;

architecture pb of ylatwhk is
  signal sawmfm : std_logic_vector(2 downto 1);
  signal orftuvov : integer;
  signal jd : std_logic_vector(2 downto 1);
  signal q : time;
  signal gnxak : integer;
  signal uzjwms : real;
begin
  jb : entity work.aipuey
    port map (godqkzdibc => uzjwms, icft => gnxak, biovwtu => q, btndhxsq => jd);
  sz : entity work.aipuey
    port map (godqkzdibc => uzjwms, icft => orftuvov, biovwtu => q, btndhxsq => sawmfm);
end pb;



entity a is
  port (monlfbdimf : inout integer_vector(3 downto 4); hzsfib : buffer character; mqgnpfvn : buffer character);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture jyo of a is
  signal gesxcogfju : std_logic;
  signal gimmgmygsr : integer;
  signal fif : time;
  signal qfpxgoof : integer;
  signal r : std_logic_vector(2 downto 1);
  signal izt : integer;
  signal typk : real;
  signal cvscyuxzjd : time;
  signal zlwuiot : boolean;
begin
  trsmxnlnzb : entity work.ylatwhk
    port map (mhtnnxjs => zlwuiot, pv => cvscyuxzjd);
  jz : entity work.aipuey
    port map (godqkzdibc => typk, icft => izt, biovwtu => cvscyuxzjd, btndhxsq => r);
  xzmqv : entity work.aipuey
    port map (godqkzdibc => typk, icft => qfpxgoof, biovwtu => cvscyuxzjd, btndhxsq => r);
  nn : entity work.fzgqmigtd
    port map (mp => fif, ujzqzip => gimmgmygsr, chou => gimmgmygsr, anqf => gesxcogfju);
end jyo;



-- Seed after: 2549307597169785582,7142793346053417159
