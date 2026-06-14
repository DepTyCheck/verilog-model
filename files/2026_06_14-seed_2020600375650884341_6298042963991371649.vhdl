-- Seed: 2020600375650884341,6298042963991371649

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (d : linkage time_vector(4 to 3); cg : linkage std_logic; ynlymwsj : linkage time_vector(3 downto 1));
end h;



architecture bwzwbuhsd of h is
  
begin
  
end bwzwbuhsd;

library ieee;
use ieee.std_logic_1164.all;

entity ic is
  port (dymo : inout integer; o : buffer std_logic; fnimp : in bit; bzjx : inout severity_level);
end ic;



architecture y of ic is
  signal txrtyrk : time_vector(3 downto 1);
  signal twdb : time_vector(3 downto 1);
  signal oew : time_vector(3 downto 1);
  signal wqith : time_vector(4 to 3);
begin
  ncaof : entity work.h
    port map (d => wqith, cg => o, ynlymwsj => oew);
  egptchz : entity work.h
    port map (d => wqith, cg => o, ynlymwsj => twdb);
  ew : entity work.h
    port map (d => wqith, cg => o, ynlymwsj => oew);
  fprm : entity work.h
    port map (d => wqith, cg => o, ynlymwsj => txrtyrk);
end y;

library ieee;
use ieee.std_logic_1164.all;

entity th is
  port (w : buffer std_logic);
end th;

library ieee;
use ieee.std_logic_1164.all;

architecture pve of th is
  signal phcb : time_vector(3 downto 1);
  signal z : std_logic;
  signal upqhptnwgi : time_vector(3 downto 1);
  signal dcr : std_logic;
  signal llbthtza : time_vector(4 to 3);
begin
  fzuntyugr : entity work.h
    port map (d => llbthtza, cg => dcr, ynlymwsj => upqhptnwgi);
  azxpqk : entity work.h
    port map (d => llbthtza, cg => z, ynlymwsj => phcb);
end pve;

library ieee;
use ieee.std_logic_1164.all;

entity tvqerdu is
  port (ievo : out std_logic; wqvgkoilrx : inout time; bjkldfgfs : in std_logic);
end tvqerdu;

library ieee;
use ieee.std_logic_1164.all;

architecture seqp of tvqerdu is
  signal jlievk : severity_level;
  signal zb : bit;
  signal bkonnpmepq : std_logic;
  signal wuafj : integer;
  signal tblctd : time_vector(3 downto 1);
  signal xumtbzkowz : std_logic;
  signal aeabivnr : time_vector(4 to 3);
begin
  vkwvsuyjnm : entity work.h
    port map (d => aeabivnr, cg => xumtbzkowz, ynlymwsj => tblctd);
  maychrnjdh : entity work.ic
    port map (dymo => wuafj, o => bkonnpmepq, fnimp => zb, bzjx => jlievk);
  flydh : entity work.th
    port map (w => ievo);
end seqp;



-- Seed after: 7307510844447031338,6298042963991371649
