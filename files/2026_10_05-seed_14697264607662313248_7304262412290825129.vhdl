-- Seed: 14697264607662313248,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity dvypbiuqc is
  port (zdpvalpvzx : inout std_logic_vector(4 to 2));
end dvypbiuqc;

architecture paunm of dvypbiuqc is
  
begin
  -- Multi-driven assignments
  zdpvalpvzx <= zdpvalpvzx;
  zdpvalpvzx <= zdpvalpvzx;
  zdpvalpvzx <= "";
end paunm;

library ieee;
use ieee.std_logic_1164.all;

entity xwiy is
  port (pqogcdq : inout std_logic; g : buffer std_logic_vector(4 to 3));
end xwiy;

architecture tlpzdoifvy of xwiy is
  
begin
  kjexrjv : entity work.dvypbiuqc
    port map (zdpvalpvzx => g);
  adkt : entity work.dvypbiuqc
    port map (zdpvalpvzx => g);
  
  -- Multi-driven assignments
  g <= g;
  g <= (others => '0');
end tlpzdoifvy;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (up : out real; ntwu : buffer std_logic_vector(3 to 1); vwzfeetcj : in real; ulqzanlaj : linkage time_vector(0 downto 4));
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture zk of p is
  signal xlxdm : std_logic_vector(4 to 3);
  signal cdsyer : std_logic;
begin
  ghapwr : entity work.dvypbiuqc
    port map (zdpvalpvzx => ntwu);
  qdxdql : entity work.dvypbiuqc
    port map (zdpvalpvzx => ntwu);
  qcmsvhzoa : entity work.xwiy
    port map (pqogcdq => cdsyer, g => xlxdm);
  
  -- Single-driven assignments
  up <= 8#6_6_0.5_6#;
  
  -- Multi-driven assignments
  xlxdm <= ntwu;
end zk;



-- Seed after: 5592446411558521668,7304262412290825129
