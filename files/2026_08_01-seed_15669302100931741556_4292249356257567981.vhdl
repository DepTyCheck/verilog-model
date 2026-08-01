-- Seed: 15669302100931741556,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity cabxhe is
  port (bevxbqw : in std_logic_vector(0 to 3); jwnwzcz : buffer integer; ypio : linkage time; yecsuwv : linkage boolean);
end cabxhe;

architecture ovqhcylh of cabxhe is
  
begin
  -- Single-driven assignments
  jwnwzcz <= 4_2_0;
end ovqhcylh;

library ieee;
use ieee.std_logic_1164.all;

entity bhblza is
  port (nkzdqpfops : linkage std_logic);
end bhblza;

library ieee;
use ieee.std_logic_1164.all;

architecture rfpxb of bhblza is
  signal nkjtc : boolean;
  signal kecr : time;
  signal fxginvj : integer;
  signal e : std_logic_vector(0 to 3);
  signal pqzrjxzy : boolean;
  signal iovqpviyns : time;
  signal qdri : integer;
  signal cx : boolean;
  signal ckd : time;
  signal aq : integer;
  signal dive : std_logic_vector(0 to 3);
begin
  f : entity work.cabxhe
    port map (bevxbqw => dive, jwnwzcz => aq, ypio => ckd, yecsuwv => cx);
  tmllmbt : entity work.cabxhe
    port map (bevxbqw => dive, jwnwzcz => qdri, ypio => iovqpviyns, yecsuwv => pqzrjxzy);
  uvgi : entity work.cabxhe
    port map (bevxbqw => e, jwnwzcz => fxginvj, ypio => kecr, yecsuwv => nkjtc);
  
  -- Multi-driven assignments
  dive <= dive;
  dive <= dive;
end rfpxb;



-- Seed after: 18170971824982571793,4292249356257567981
