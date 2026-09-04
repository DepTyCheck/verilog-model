-- Seed: 403914990459583144,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity jimt is
  port (zknzsutmx : linkage std_logic_vector(3 downto 4); ewc : out time);
end jimt;

architecture embud of jimt is
  
begin
  -- Single-driven assignments
  ewc <= 8#1.7# ps;
end embud;

entity kahfhkfzr is
  port (fpsprfkqsh : in time_vector(2 to 2); paovy : linkage bit_vector(1 downto 3); qce : buffer bit; iynjgyg : in boolean_vector(0 to 2));
end kahfhkfzr;

library ieee;
use ieee.std_logic_1164.all;

architecture kwrbxha of kahfhkfzr is
  signal jjwtjkxm : time;
  signal gyjokui : time;
  signal pfeunx : std_logic_vector(3 downto 4);
  signal irw : time;
  signal hukgcayijo : std_logic_vector(3 downto 4);
  signal tqdgw : time;
  signal idvzqtt : std_logic_vector(3 downto 4);
begin
  d : entity work.jimt
    port map (zknzsutmx => idvzqtt, ewc => tqdgw);
  gciholre : entity work.jimt
    port map (zknzsutmx => hukgcayijo, ewc => irw);
  plkxvn : entity work.jimt
    port map (zknzsutmx => pfeunx, ewc => gyjokui);
  jeyamqprz : entity work.jimt
    port map (zknzsutmx => idvzqtt, ewc => jjwtjkxm);
  
  -- Single-driven assignments
  qce <= qce;
end kwrbxha;

library ieee;
use ieee.std_logic_1164.all;

entity aec is
  port (vpgbxnjg : linkage boolean; kqgfsjzxe : linkage std_logic);
end aec;

library ieee;
use ieee.std_logic_1164.all;

architecture bkbaauiqv of aec is
  signal z : boolean_vector(0 to 2);
  signal snbaihaau : bit;
  signal vok : bit_vector(1 downto 3);
  signal ijgopjzlkb : time_vector(2 to 2);
  signal fyaswxryp : time;
  signal uyc : time;
  signal fosrvfnrhf : std_logic_vector(3 downto 4);
  signal vfgg : time;
  signal e : std_logic_vector(3 downto 4);
begin
  aivqtwon : entity work.jimt
    port map (zknzsutmx => e, ewc => vfgg);
  lncyliecm : entity work.jimt
    port map (zknzsutmx => fosrvfnrhf, ewc => uyc);
  siww : entity work.jimt
    port map (zknzsutmx => fosrvfnrhf, ewc => fyaswxryp);
  ilgiwan : entity work.kahfhkfzr
    port map (fpsprfkqsh => ijgopjzlkb, paovy => vok, qce => snbaihaau, iynjgyg => z);
end bkbaauiqv;



-- Seed after: 12354009195342061298,4404421571376382767
