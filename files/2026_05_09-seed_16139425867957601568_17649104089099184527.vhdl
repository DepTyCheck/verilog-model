-- Seed: 16139425867957601568,17649104089099184527

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (yaca : inout time; vseuocea : linkage integer; ysnsstipxl : linkage time; zzbcyrng : inout std_logic);
end s;



architecture nzsefgw of s is
  
begin
  
end nzsefgw;

library ieee;
use ieee.std_logic_1164.all;

entity uedappq is
  port (w : out real; gh : buffer std_logic);
end uedappq;

library ieee;
use ieee.std_logic_1164.all;

architecture ybjzhjkrmf of uedappq is
  signal ergohyh : time;
  signal rapx : time;
  signal tllkjzjklv : std_logic;
  signal lbtinhrojb : time;
  signal ajmknizlfd : integer;
  signal xunpo : time;
begin
  ozsgtt : entity work.s
    port map (yaca => xunpo, vseuocea => ajmknizlfd, ysnsstipxl => lbtinhrojb, zzbcyrng => tllkjzjklv);
  kupvyuwa : entity work.s
    port map (yaca => rapx, vseuocea => ajmknizlfd, ysnsstipxl => ergohyh, zzbcyrng => gh);
end ybjzhjkrmf;



entity drzy is
  port (bocaufngup : inout bit; qkciobd : out time; sizhokwima : linkage time; tagjr : inout time);
end drzy;

library ieee;
use ieee.std_logic_1164.all;

architecture lwtttxc of drzy is
  signal zkzavzv : std_logic;
  signal cgxu : real;
begin
  wp : entity work.uedappq
    port map (w => cgxu, gh => zkzavzv);
end lwtttxc;



entity v is
  port (zuvddq : inout integer);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture aokd of v is
  signal ankvwr : std_logic;
  signal bexyxo : std_logic;
  signal tsirpqj : time;
  signal xfsvjdoa : time;
begin
  gefvhob : entity work.s
    port map (yaca => xfsvjdoa, vseuocea => zuvddq, ysnsstipxl => tsirpqj, zzbcyrng => bexyxo);
  tihpjtrlwp : entity work.s
    port map (yaca => tsirpqj, vseuocea => zuvddq, ysnsstipxl => xfsvjdoa, zzbcyrng => ankvwr);
end aokd;



-- Seed after: 3454108554484455105,17649104089099184527
