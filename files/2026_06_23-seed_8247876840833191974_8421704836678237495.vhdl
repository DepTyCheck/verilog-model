-- Seed: 8247876840833191974,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity zcslax is
  port (wyjwnn : in std_logic; xas : buffer time; ctylsdrj : linkage real_vector(2 downto 3));
end zcslax;

architecture exkcfvpzt of zcslax is
  
begin
  -- Single-driven assignments
  xas <= 1343.4432 us;
end exkcfvpzt;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (q : in integer; z : linkage bit_vector(3 to 1); qsueifuxg : buffer std_logic);
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture jf of x is
  signal zunqp : real_vector(2 downto 3);
  signal a : time;
  signal p : std_logic;
  signal lpaolw : real_vector(2 downto 3);
  signal ei : time;
  signal etb : std_logic;
  signal deltodso : real_vector(2 downto 3);
  signal kcxxiwlucy : time;
  signal hj : real_vector(2 downto 3);
  signal b : time;
  signal ltcvpwhr : std_logic;
begin
  lzzo : entity work.zcslax
    port map (wyjwnn => ltcvpwhr, xas => b, ctylsdrj => hj);
  zkcdboojp : entity work.zcslax
    port map (wyjwnn => ltcvpwhr, xas => kcxxiwlucy, ctylsdrj => deltodso);
  f : entity work.zcslax
    port map (wyjwnn => etb, xas => ei, ctylsdrj => lpaolw);
  v : entity work.zcslax
    port map (wyjwnn => p, xas => a, ctylsdrj => zunqp);
  
  -- Multi-driven assignments
  etb <= 'H';
  qsueifuxg <= 'L';
  etb <= '1';
end jf;

entity tgucy is
  port (ynqil : linkage time; s : out real_vector(0 downto 0));
end tgucy;

library ieee;
use ieee.std_logic_1164.all;

architecture xjqxdx of tgucy is
  signal npqai : real_vector(2 downto 3);
  signal ksbwfdooja : time;
  signal a : real_vector(2 downto 3);
  signal dmzx : time;
  signal ksrxdzxg : std_logic;
  signal xvoq : real_vector(2 downto 3);
  signal fzgi : time;
  signal gtceb : std_logic;
begin
  vabwhu : entity work.zcslax
    port map (wyjwnn => gtceb, xas => fzgi, ctylsdrj => xvoq);
  zei : entity work.zcslax
    port map (wyjwnn => ksrxdzxg, xas => dmzx, ctylsdrj => a);
  wtjjkkryr : entity work.zcslax
    port map (wyjwnn => gtceb, xas => ksbwfdooja, ctylsdrj => npqai);
  
  -- Single-driven assignments
  s <= (others => 2#10.11001#);
  
  -- Multi-driven assignments
  gtceb <= 'Z';
  gtceb <= '-';
  gtceb <= '-';
  gtceb <= '0';
end xjqxdx;

entity y is
  port (jma : in time; m : inout integer; hetfap : in integer);
end y;

library ieee;
use ieee.std_logic_1164.all;

architecture qcct of y is
  signal waratvbxr : real_vector(2 downto 3);
  signal o : time;
  signal jrbb : std_logic;
begin
  tco : entity work.zcslax
    port map (wyjwnn => jrbb, xas => o, ctylsdrj => waratvbxr);
  
  -- Single-driven assignments
  m <= 8#5#;
end qcct;



-- Seed after: 6420103667867933152,8421704836678237495
