-- Seed: 2611677228263472711,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity kyryp is
  port (kjjdrckhhw : in time; ubakew : buffer std_logic; tj : linkage std_logic; ikxnypte : in real);
end kyryp;

architecture y of kyryp is
  
begin
  -- Multi-driven assignments
  ubakew <= 'L';
  ubakew <= 'W';
  ubakew <= 'Z';
  ubakew <= ubakew;
end y;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (phqs : buffer std_logic; xkqi : in std_logic_vector(2 downto 0));
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture dq of w is
  signal vcpgidpjv : std_logic;
  signal wizqzn : std_logic;
  signal hck : real;
  signal tkx : std_logic;
  signal axwugz : time;
  signal vhkqel : real;
  signal xton : std_logic;
  signal e : std_logic;
  signal ib : time;
  signal drzvpn : real;
  signal vvrdr : std_logic;
  signal yv : time;
begin
  nxoo : entity work.kyryp
    port map (kjjdrckhhw => yv, ubakew => phqs, tj => vvrdr, ikxnypte => drzvpn);
  wvgw : entity work.kyryp
    port map (kjjdrckhhw => ib, ubakew => e, tj => xton, ikxnypte => vhkqel);
  fzvchpx : entity work.kyryp
    port map (kjjdrckhhw => axwugz, ubakew => phqs, tj => tkx, ikxnypte => hck);
  j : entity work.kyryp
    port map (kjjdrckhhw => ib, ubakew => wizqzn, tj => vcpgidpjv, ikxnypte => drzvpn);
  
  -- Single-driven assignments
  yv <= 2#0.0# us;
  vhkqel <= 8#6215.5_1_3_1#;
  ib <= 3 min;
  
  -- Multi-driven assignments
  xton <= 'L';
  phqs <= phqs;
  wizqzn <= '-';
end dq;

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (yidbejshps : out integer_vector(0 to 3); kml : out real; amdi : out std_logic; cyuia : linkage real_vector(3 downto 0));
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture qgqwj of i is
  signal dkccvh : std_logic;
  signal dbjovvclbc : time;
  signal siwedknr : std_logic;
  signal lgfibi : time;
  signal pvhhlyeuai : std_logic_vector(2 downto 0);
  signal pvoxzo : real;
  signal stkf : std_logic;
  signal ur : time;
begin
  oueyxbcchs : entity work.kyryp
    port map (kjjdrckhhw => ur, ubakew => stkf, tj => amdi, ikxnypte => pvoxzo);
  ksdcrx : entity work.w
    port map (phqs => amdi, xkqi => pvhhlyeuai);
  xab : entity work.kyryp
    port map (kjjdrckhhw => lgfibi, ubakew => stkf, tj => siwedknr, ikxnypte => pvoxzo);
  onsrwuhy : entity work.kyryp
    port map (kjjdrckhhw => dbjovvclbc, ubakew => dkccvh, tj => siwedknr, ikxnypte => kml);
  
  -- Single-driven assignments
  ur <= ur;
  kml <= kml;
  dbjovvclbc <= ur;
  lgfibi <= 16#9D1# fs;
  
  -- Multi-driven assignments
  amdi <= 'U';
  amdi <= stkf;
end qgqwj;



-- Seed after: 11180379730100242739,7808623373429384027
