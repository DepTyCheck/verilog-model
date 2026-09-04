-- Seed: 9953537073958678815,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity gsk is
  port (euekz : buffer time_vector(0 to 4); hdqyeibqz : buffer std_logic; omgrdhj : buffer std_logic_vector(2 downto 4); dt : out integer);
end gsk;

architecture vbopzdiryc of gsk is
  
begin
  -- Single-driven assignments
  dt <= 10123;
  euekz <= euekz;
  
  -- Multi-driven assignments
  omgrdhj <= "";
  omgrdhj <= omgrdhj;
  hdqyeibqz <= '0';
end vbopzdiryc;

entity d is
  port (ktmpf : linkage real);
end d;

library ieee;
use ieee.std_logic_1164.all;

architecture sl of d is
  signal hlbcxdugip : integer;
  signal ndnq : std_logic_vector(2 downto 4);
  signal nvozeipk : std_logic;
  signal rgksb : time_vector(0 to 4);
  signal hvabqb : integer;
  signal yyoev : std_logic_vector(2 downto 4);
  signal t : time_vector(0 to 4);
  signal vrwvkladay : integer;
  signal xdfle : std_logic_vector(2 downto 4);
  signal rr : std_logic;
  signal xwroc : time_vector(0 to 4);
begin
  dm : entity work.gsk
    port map (euekz => xwroc, hdqyeibqz => rr, omgrdhj => xdfle, dt => vrwvkladay);
  zegeq : entity work.gsk
    port map (euekz => t, hdqyeibqz => rr, omgrdhj => yyoev, dt => hvabqb);
  iacdqb : entity work.gsk
    port map (euekz => rgksb, hdqyeibqz => nvozeipk, omgrdhj => ndnq, dt => hlbcxdugip);
  
  -- Multi-driven assignments
  ndnq <= (others => '0');
  nvozeipk <= 'U';
end sl;

library ieee;
use ieee.std_logic_1164.all;

entity jnlkucsuv is
  port (vud : linkage std_logic; imjcvd : buffer boolean);
end jnlkucsuv;

architecture lfrmlkcz of jnlkucsuv is
  
begin
  
end lfrmlkcz;

library ieee;
use ieee.std_logic_1164.all;

entity ogbnfkby is
  port (qehyfjmyn : inout severity_level; qwqenpidn : inout real; ftwiqsis : buffer std_logic);
end ogbnfkby;

library ieee;
use ieee.std_logic_1164.all;

architecture gtwukq of ogbnfkby is
  signal vhusvse : integer;
  signal ufjwuwda : std_logic_vector(2 downto 4);
  signal fiwac : std_logic;
  signal iodyhxmvp : time_vector(0 to 4);
  signal uvlwv : real;
  signal ypffqkikt : real;
  signal wrx : boolean;
begin
  ldf : entity work.jnlkucsuv
    port map (vud => ftwiqsis, imjcvd => wrx);
  wvhgrd : entity work.d
    port map (ktmpf => ypffqkikt);
  p : entity work.d
    port map (ktmpf => uvlwv);
  psegrwxoz : entity work.gsk
    port map (euekz => iodyhxmvp, hdqyeibqz => fiwac, omgrdhj => ufjwuwda, dt => vhusvse);
  
  -- Single-driven assignments
  qwqenpidn <= 311.034;
  
  -- Multi-driven assignments
  ufjwuwda <= ufjwuwda;
end gtwukq;



-- Seed after: 17755842903779921153,4404421571376382767
