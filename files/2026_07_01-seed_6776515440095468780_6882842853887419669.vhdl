-- Seed: 6776515440095468780,6882842853887419669

entity afrpxs is
  port (yhsctj : inout time);
end afrpxs;

architecture ukqnckgfik of afrpxs is
  
begin
  
end ukqnckgfik;

library ieee;
use ieee.std_logic_1164.all;

entity yp is
  port (zbqr : linkage integer; revxsxh : in time; bdzd : in std_logic);
end yp;

architecture gschuo of yp is
  signal qgauygc : time;
  signal xtb : time;
  signal ruje : time;
  signal mumsd : time;
begin
  bsuqsamo : entity work.afrpxs
    port map (yhsctj => mumsd);
  lzrbkscp : entity work.afrpxs
    port map (yhsctj => ruje);
  feswpaili : entity work.afrpxs
    port map (yhsctj => xtb);
  ijzibi : entity work.afrpxs
    port map (yhsctj => qgauygc);
end gschuo;

library ieee;
use ieee.std_logic_1164.all;

entity svej is
  port (xnmd : out std_logic; bohsn : buffer std_logic_vector(1 to 0); jacvpoxr : inout std_logic; pohj : in time);
end svej;

library ieee;
use ieee.std_logic_1164.all;

architecture uovtijjc of svej is
  signal javdjjzuu : std_logic;
  signal wzodn : integer;
  signal rhgmo : time;
  signal tr : time;
begin
  tpalnk : entity work.afrpxs
    port map (yhsctj => tr);
  now : entity work.afrpxs
    port map (yhsctj => rhgmo);
  lymfh : entity work.yp
    port map (zbqr => wzodn, revxsxh => tr, bdzd => javdjjzuu);
  
  -- Multi-driven assignments
  jacvpoxr <= 'U';
end uovtijjc;



-- Seed after: 13348055715456362532,6882842853887419669
