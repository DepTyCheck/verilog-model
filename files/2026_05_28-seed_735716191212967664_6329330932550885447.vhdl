-- Seed: 735716191212967664,6329330932550885447

library ieee;
use ieee.std_logic_1164.all;

entity ipn is
  port (jmcepzfay : out std_logic; sd : in character; egztdamawx : inout std_logic_vector(2 to 2));
end ipn;



architecture tbjl of ipn is
  
begin
  
end tbjl;



entity y is
  port (sbymegbdxq : inout real_vector(1 to 0));
end y;

library ieee;
use ieee.std_logic_1164.all;

architecture zsu of y is
  signal wmaadys : character;
  signal qmiforxkw : std_logic;
  signal sgxdfci : std_logic;
  signal wnqcjmgu : std_logic_vector(2 to 2);
  signal mdvny : character;
  signal myzdyev : std_logic;
begin
  ycdrtp : entity work.ipn
    port map (jmcepzfay => myzdyev, sd => mdvny, egztdamawx => wnqcjmgu);
  pbnjoslt : entity work.ipn
    port map (jmcepzfay => sgxdfci, sd => mdvny, egztdamawx => wnqcjmgu);
  dhithlz : entity work.ipn
    port map (jmcepzfay => qmiforxkw, sd => wmaadys, egztdamawx => wnqcjmgu);
end zsu;



entity cenidxxcc is
  port (x : buffer real; zyhmma : inout time);
end cenidxxcc;

library ieee;
use ieee.std_logic_1164.all;

architecture rcs of cenidxxcc is
  signal ltoqd : std_logic_vector(2 to 2);
  signal etbtrcym : character;
  signal lloqk : std_logic;
  signal mcfrtrs : real_vector(1 to 0);
begin
  enj : entity work.y
    port map (sbymegbdxq => mcfrtrs);
  asymoqdr : entity work.ipn
    port map (jmcepzfay => lloqk, sd => etbtrcym, egztdamawx => ltoqd);
end rcs;



-- Seed after: 18444081240621960690,6329330932550885447
