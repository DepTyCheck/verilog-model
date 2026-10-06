-- Seed: 10681554432674918085,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity ovzqlng is
  port (vikemxor : linkage std_logic_vector(0 downto 3); trlibpmsx : linkage integer);
end ovzqlng;

architecture pwblbejxw of ovzqlng is
  
begin
  
end pwblbejxw;

library ieee;
use ieee.std_logic_1164.all;

entity gwhk is
  port (ptolhqbj : inout std_logic; rhieewt : buffer real; rpx : inout real);
end gwhk;

library ieee;
use ieee.std_logic_1164.all;

architecture qytyvkr of gwhk is
  signal asjbqpwbwd : integer;
  signal ibqy : std_logic_vector(0 downto 3);
  signal za : integer;
  signal trmtag : std_logic_vector(0 downto 3);
  signal tteswqay : integer;
  signal xzcyeen : std_logic_vector(0 downto 3);
begin
  glagpnlro : entity work.ovzqlng
    port map (vikemxor => xzcyeen, trlibpmsx => tteswqay);
  crfniqq : entity work.ovzqlng
    port map (vikemxor => trmtag, trlibpmsx => za);
  jodkwk : entity work.ovzqlng
    port map (vikemxor => ibqy, trlibpmsx => asjbqpwbwd);
  
  -- Single-driven assignments
  rpx <= 8#5_4_6_0_2.3_3#;
  rhieewt <= rpx;
  
  -- Multi-driven assignments
  ptolhqbj <= 'Z';
  ptolhqbj <= 'L';
end qytyvkr;

entity cdxqoussw is
  port (rfgzn : out time; yfnaemmiw : buffer integer; mpb : out time; wrm : buffer severity_level);
end cdxqoussw;

library ieee;
use ieee.std_logic_1164.all;

architecture lsibgyddus of cdxqoussw is
  signal rgdq : real;
  signal naxsfd : real;
  signal yfpquqvipx : std_logic;
  signal govlagws : integer;
  signal mgltl : std_logic_vector(0 downto 3);
begin
  ci : entity work.ovzqlng
    port map (vikemxor => mgltl, trlibpmsx => yfnaemmiw);
  q : entity work.ovzqlng
    port map (vikemxor => mgltl, trlibpmsx => govlagws);
  ybk : entity work.gwhk
    port map (ptolhqbj => yfpquqvipx, rhieewt => naxsfd, rpx => rgdq);
  
  -- Multi-driven assignments
  mgltl <= "";
  mgltl <= (others => '0');
  yfpquqvipx <= '0';
  mgltl <= (others => '0');
end lsibgyddus;



-- Seed after: 14189966048337671274,3042374792655995433
