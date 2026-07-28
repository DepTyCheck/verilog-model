-- Seed: 1004462671575984916,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity jdchv is
  port (wuefg : out time; bgdonu : inout std_logic_vector(4 to 0); saq : inout bit; d : out std_logic);
end jdchv;

architecture apif of jdchv is
  
begin
  -- Single-driven assignments
  saq <= '0';
  wuefg <= wuefg;
  
  -- Multi-driven assignments
  bgdonu <= (others => '0');
  d <= d;
  d <= d;
  d <= 'U';
end apif;

entity grpby is
  port (zwqemplrzi : linkage time; ktr : out real; yddx : in bit; nr : inout real);
end grpby;

library ieee;
use ieee.std_logic_1164.all;

architecture ga of grpby is
  signal hlgkwhmgqs : bit;
  signal yjuwuq : std_logic_vector(4 to 0);
  signal ihuoniguha : time;
  signal hbjyllru : std_logic;
  signal wymmrx : bit;
  signal ms : time;
  signal yj : bit;
  signal pdi : time;
  signal ah : std_logic;
  signal nhbd : bit;
  signal zbvfw : std_logic_vector(4 to 0);
  signal xphtsy : time;
begin
  x : entity work.jdchv
    port map (wuefg => xphtsy, bgdonu => zbvfw, saq => nhbd, d => ah);
  pxqd : entity work.jdchv
    port map (wuefg => pdi, bgdonu => zbvfw, saq => yj, d => ah);
  zhutwh : entity work.jdchv
    port map (wuefg => ms, bgdonu => zbvfw, saq => wymmrx, d => hbjyllru);
  issgsxgt : entity work.jdchv
    port map (wuefg => ihuoniguha, bgdonu => yjuwuq, saq => hlgkwhmgqs, d => ah);
  
  -- Multi-driven assignments
  yjuwuq <= (others => '0');
end ga;

library ieee;
use ieee.std_logic_1164.all;

entity jvigeuesvs is
  port (mep : in time; lk : out real; gq : out std_logic_vector(2 downto 3); qritorva : linkage std_logic_vector(0 downto 1));
end jvigeuesvs;

architecture g of jvigeuesvs is
  signal abobx : bit;
  signal edgyleca : real;
  signal sjvhxyk : time;
  signal qbp : real;
  signal xmvgbqiw : bit;
  signal uzukofokz : real;
  signal awem : time;
begin
  rnmsaz : entity work.grpby
    port map (zwqemplrzi => awem, ktr => uzukofokz, yddx => xmvgbqiw, nr => qbp);
  i : entity work.grpby
    port map (zwqemplrzi => sjvhxyk, ktr => edgyleca, yddx => abobx, nr => lk);
  
  -- Single-driven assignments
  xmvgbqiw <= xmvgbqiw;
  
  -- Multi-driven assignments
  gq <= gq;
end g;



-- Seed after: 59836648348671844,2511821214772927453
