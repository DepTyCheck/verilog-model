-- Seed: 17009506583776507667,13903879141658024201

library ieee;
use ieee.std_logic_1164.all;

entity bgdweml is
  port (yzqt : inout std_logic_vector(1 downto 2); ogz : linkage integer_vector(3 to 2); siodoqp : inout integer_vector(4 downto 0));
end bgdweml;



architecture iihwtro of bgdweml is
  
begin
  
end iihwtro;

library ieee;
use ieee.std_logic_1164.all;

entity cqkov is
  port (ezwxelfg : out bit; y : out std_logic);
end cqkov;

library ieee;
use ieee.std_logic_1164.all;

architecture rhihbrysjc of cqkov is
  signal etco : integer_vector(4 downto 0);
  signal iinzle : integer_vector(3 to 2);
  signal tmpm : std_logic_vector(1 downto 2);
  signal jsk : integer_vector(4 downto 0);
  signal kngzpelqir : integer_vector(3 to 2);
  signal rylzwuj : std_logic_vector(1 downto 2);
begin
  xivsbvzvh : entity work.bgdweml
    port map (yzqt => rylzwuj, ogz => kngzpelqir, siodoqp => jsk);
  pwgxxi : entity work.bgdweml
    port map (yzqt => tmpm, ogz => iinzle, siodoqp => etco);
end rhihbrysjc;



entity mjhuwlncd is
  port (eovbqhqkfi : buffer boolean);
end mjhuwlncd;

library ieee;
use ieee.std_logic_1164.all;

architecture ruhh of mjhuwlncd is
  signal mpv : integer_vector(4 downto 0);
  signal xrzsimpl : integer_vector(3 to 2);
  signal emieuyznmi : std_logic_vector(1 downto 2);
begin
  bhrdrjje : entity work.bgdweml
    port map (yzqt => emieuyznmi, ogz => xrzsimpl, siodoqp => mpv);
end ruhh;



-- Seed after: 7863185907647290147,13903879141658024201
