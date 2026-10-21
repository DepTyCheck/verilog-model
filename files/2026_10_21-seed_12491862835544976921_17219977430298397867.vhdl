-- Seed: 12491862835544976921,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity th is
  port (zs : buffer std_logic_vector(2 to 4); lhrraht : buffer integer);
end th;

architecture nbpprorjxc of th is
  
begin
  -- Single-driven assignments
  lhrraht <= 8#3055#;
  
  -- Multi-driven assignments
  zs <= "00L";
end nbpprorjxc;

library ieee;
use ieee.std_logic_1164.all;

entity kszfilz is
  port (affr : out std_logic_vector(0 to 2));
end kszfilz;

library ieee;
use ieee.std_logic_1164.all;

architecture ndwpisv of kszfilz is
  signal qqoigzaj : integer;
  signal calfzaoqa : std_logic_vector(2 to 4);
  signal jc : integer;
  signal strkqj : std_logic_vector(2 to 4);
  signal fljzwuhhxj : integer;
  signal rwxc : std_logic_vector(2 to 4);
begin
  ndliwvlz : entity work.th
    port map (zs => rwxc, lhrraht => fljzwuhhxj);
  nhvhgx : entity work.th
    port map (zs => strkqj, lhrraht => jc);
  hhqtw : entity work.th
    port map (zs => calfzaoqa, lhrraht => qqoigzaj);
  
  -- Multi-driven assignments
  calfzaoqa <= rwxc;
  affr <= affr;
  rwxc <= rwxc;
  rwxc <= ('1', 'X', '0');
end ndwpisv;

entity zrw is
  port (uh : linkage boolean);
end zrw;

library ieee;
use ieee.std_logic_1164.all;

architecture p of zrw is
  signal d : integer;
  signal ii : std_logic_vector(2 to 4);
begin
  urmcgu : entity work.th
    port map (zs => ii, lhrraht => d);
  
  -- Multi-driven assignments
  ii <= "XWX";
  ii <= "UXX";
end p;



-- Seed after: 4119123284444559882,17219977430298397867
