-- Seed: 14967626089192946069,13694093582652240945

entity sm is
  port (ixagtd : buffer bit; axxzeqrnjn : out time; n : inout time_vector(1 downto 1));
end sm;

architecture kvjlswqngv of sm is
  
begin
  -- Single-driven assignments
  n <= (others => 8#32466# ms);
  axxzeqrnjn <= 16#3AF# ms;
  ixagtd <= '0';
end kvjlswqngv;

library ieee;
use ieee.std_logic_1164.all;

entity lfc is
  port (srg : buffer std_logic; xubjcim : linkage integer; kymkettmw : in time);
end lfc;

architecture lhszhevgk of lfc is
  signal efaniwoqxz : time_vector(1 downto 1);
  signal dk : time;
  signal rtarhayvf : bit;
  signal eishcm : time_vector(1 downto 1);
  signal ou : time;
  signal kfiz : bit;
  signal omvbzqlqkd : time_vector(1 downto 1);
  signal qxvtruskh : time;
  signal tkbvrbemv : bit;
begin
  xffhg : entity work.sm
    port map (ixagtd => tkbvrbemv, axxzeqrnjn => qxvtruskh, n => omvbzqlqkd);
  sol : entity work.sm
    port map (ixagtd => kfiz, axxzeqrnjn => ou, n => eishcm);
  b : entity work.sm
    port map (ixagtd => rtarhayvf, axxzeqrnjn => dk, n => efaniwoqxz);
  
  -- Multi-driven assignments
  srg <= '1';
  srg <= 'L';
  srg <= 'W';
  srg <= 'U';
end lhszhevgk;

library ieee;
use ieee.std_logic_1164.all;

entity bxwv is
  port (f : buffer std_logic_vector(0 to 2); kdggw : out integer; o : out boolean);
end bxwv;

library ieee;
use ieee.std_logic_1164.all;

architecture mz of bxwv is
  signal fqrbfgi : time;
  signal qqdtl : std_logic;
begin
  xd : entity work.lfc
    port map (srg => qqdtl, xubjcim => kdggw, kymkettmw => fqrbfgi);
  
  -- Single-driven assignments
  fqrbfgi <= 34.4_2_1 us;
  o <= TRUE;
  
  -- Multi-driven assignments
  f <= "0XU";
  f <= "W1H";
end mz;

library ieee;
use ieee.std_logic_1164.all;

entity npndydi is
  port ( maza : in time_vector(0 to 2)
  ; scevdppr : out std_logic_vector(3 to 3)
  ; pqqpj : inout std_logic_vector(3 downto 3)
  ; fowrnfmy : buffer std_logic_vector(3 downto 0)
  );
end npndydi;

architecture bocepbczz of npndydi is
  signal zvsr : time_vector(1 downto 1);
  signal vh : time;
  signal mocj : bit;
  signal ppol : time_vector(1 downto 1);
  signal quqtmjc : time;
  signal qhkr : bit;
begin
  ujq : entity work.sm
    port map (ixagtd => qhkr, axxzeqrnjn => quqtmjc, n => ppol);
  inb : entity work.sm
    port map (ixagtd => mocj, axxzeqrnjn => vh, n => zvsr);
  
  -- Multi-driven assignments
  pqqpj <= "-";
  pqqpj <= (others => 'X');
end bocepbczz;



-- Seed after: 15667103044057463366,13694093582652240945
