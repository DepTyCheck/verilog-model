-- Seed: 10367429044388162870,390128806780030455



entity lkzychg is
  port (uobwszpijq : linkage real);
end lkzychg;



architecture d of lkzychg is
  
begin
  
end d;

library ieee;
use ieee.std_logic_1164.all;

entity oheqpb is
  port (pfngr : linkage std_logic; pnlkoug : linkage time; zcshsu : linkage real);
end oheqpb;



architecture sbbre of oheqpb is
  signal gllkebmqh : real;
  signal ptczbjsj : real;
begin
  gvaftjfak : entity work.lkzychg
    port map (uobwszpijq => zcshsu);
  ibedneb : entity work.lkzychg
    port map (uobwszpijq => zcshsu);
  azkfxsbn : entity work.lkzychg
    port map (uobwszpijq => ptczbjsj);
  ato : entity work.lkzychg
    port map (uobwszpijq => gllkebmqh);
end sbbre;

library ieee;
use ieee.std_logic_1164.all;

entity davpemp is
  port (yltqxrzy : out std_logic; uuz : in time; oyju : in std_logic; ynqgq : linkage time);
end davpemp;

library ieee;
use ieee.std_logic_1164.all;

architecture iq of davpemp is
  signal vkoklfylz : std_logic;
  signal tc : real;
  signal o : real;
  signal qsv : real;
begin
  wowurtemew : entity work.lkzychg
    port map (uobwszpijq => qsv);
  sm : entity work.oheqpb
    port map (pfngr => oyju, pnlkoug => ynqgq, zcshsu => o);
  yjwvbnbeo : entity work.lkzychg
    port map (uobwszpijq => tc);
  hy : entity work.oheqpb
    port map (pfngr => vkoklfylz, pnlkoug => uuz, zcshsu => o);
end iq;

library ieee;
use ieee.std_logic_1164.all;

entity ro is
  port (sfr : buffer integer; wxd : in std_logic);
end ro;

library ieee;
use ieee.std_logic_1164.all;

architecture ghed of ro is
  signal rz : real;
  signal wn : time;
  signal pmwdccj : std_logic;
  signal ptlgt : std_logic;
  signal bvuefhzw : std_logic;
  signal wlksmlbxxt : time;
  signal b : std_logic;
begin
  ohzccpazld : entity work.davpemp
    port map (yltqxrzy => b, uuz => wlksmlbxxt, oyju => bvuefhzw, ynqgq => wlksmlbxxt);
  cdabqz : entity work.davpemp
    port map (yltqxrzy => ptlgt, uuz => wlksmlbxxt, oyju => b, ynqgq => wlksmlbxxt);
  rbyxi : entity work.davpemp
    port map (yltqxrzy => pmwdccj, uuz => wlksmlbxxt, oyju => wxd, ynqgq => wn);
  ggplubk : entity work.lkzychg
    port map (uobwszpijq => rz);
end ghed;



-- Seed after: 9338713583143531764,390128806780030455
