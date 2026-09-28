-- Seed: 6367603317327135525,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity iv is
  port (sa : linkage integer_vector(3 downto 2); iusr : out std_logic_vector(3 to 2); tgq : in std_logic_vector(4 to 0); whgimh : in real);
end iv;

architecture tfheta of iv is
  
begin
  -- Multi-driven assignments
  iusr <= (others => '0');
  iusr <= tgq;
  iusr <= "";
  iusr <= tgq;
end tfheta;

entity oaibsjfyu is
  port (uzxtquhdzg : inout time; pzotf : linkage integer; usziqbudyf : linkage real; rxjliunz : linkage boolean);
end oaibsjfyu;

library ieee;
use ieee.std_logic_1164.all;

architecture kdegnwyjt of oaibsjfyu is
  signal w : std_logic_vector(3 to 2);
  signal qhhywm : integer_vector(3 downto 2);
  signal zipv : real;
  signal szvkiacgsn : std_logic_vector(4 to 0);
  signal xlikbwj : std_logic_vector(4 to 0);
  signal r : integer_vector(3 downto 2);
  signal tr : real;
  signal vlz : std_logic_vector(4 to 0);
  signal ampefwnik : std_logic_vector(3 to 2);
  signal fvxbsaxgw : integer_vector(3 downto 2);
begin
  vcgkkgoafq : entity work.iv
    port map (sa => fvxbsaxgw, iusr => ampefwnik, tgq => vlz, whgimh => tr);
  gnwjtx : entity work.iv
    port map (sa => r, iusr => xlikbwj, tgq => szvkiacgsn, whgimh => zipv);
  ogf : entity work.iv
    port map (sa => qhhywm, iusr => w, tgq => xlikbwj, whgimh => tr);
  
  -- Single-driven assignments
  tr <= 8#1120.7#;
  zipv <= tr;
  uzxtquhdzg <= 0 sec;
end kdegnwyjt;

entity hluobcvd is
  port (z : out boolean; j : linkage real_vector(1 downto 3); afikf : out bit_vector(1 to 0));
end hluobcvd;

library ieee;
use ieee.std_logic_1164.all;

architecture gpwgjxlb of hluobcvd is
  signal vfg : boolean;
  signal au : real;
  signal scqqg : integer;
  signal cflj : time;
  signal whpbwddl : integer_vector(3 downto 2);
  signal rrspsrjoa : real;
  signal ocfncvi : std_logic_vector(4 to 0);
  signal ztjrk : std_logic_vector(4 to 0);
  signal pjngvkardm : integer_vector(3 downto 2);
begin
  zcasad : entity work.iv
    port map (sa => pjngvkardm, iusr => ztjrk, tgq => ocfncvi, whgimh => rrspsrjoa);
  aw : entity work.iv
    port map (sa => whpbwddl, iusr => ztjrk, tgq => ztjrk, whgimh => rrspsrjoa);
  njusbe : entity work.oaibsjfyu
    port map (uzxtquhdzg => cflj, pzotf => scqqg, usziqbudyf => au, rxjliunz => vfg);
  
  -- Multi-driven assignments
  ztjrk <= ztjrk;
end gpwgjxlb;



-- Seed after: 8067023992991314251,7311216359267151659
