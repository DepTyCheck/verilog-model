-- Seed: 17287425381989380219,5444032936641997477

entity zwpfzefuko is
  port (sgco : buffer boolean; mtmx : inout integer_vector(2 downto 3));
end zwpfzefuko;

architecture bmri of zwpfzefuko is
  
begin
  -- Single-driven assignments
  mtmx <= (others => 0);
  sgco <= sgco;
end bmri;

library ieee;
use ieee.std_logic_1164.all;

entity llrpnveke is
  port (wfuqul : linkage std_logic_vector(0 to 0));
end llrpnveke;

architecture ryqsyy of llrpnveke is
  signal unmh : integer_vector(2 downto 3);
  signal wnqylmuaa : boolean;
  signal atpj : integer_vector(2 downto 3);
  signal kukbtw : boolean;
  signal ptwgtys : integer_vector(2 downto 3);
  signal gt : boolean;
  signal afig : integer_vector(2 downto 3);
  signal ix : boolean;
begin
  b : entity work.zwpfzefuko
    port map (sgco => ix, mtmx => afig);
  f : entity work.zwpfzefuko
    port map (sgco => gt, mtmx => ptwgtys);
  nsubvpwm : entity work.zwpfzefuko
    port map (sgco => kukbtw, mtmx => atpj);
  uxmaamz : entity work.zwpfzefuko
    port map (sgco => wnqylmuaa, mtmx => unmh);
end ryqsyy;

entity tpfzzmxas is
  port (gbimsg : linkage character; vrrcv : inout boolean_vector(1 to 2); h : buffer bit);
end tpfzzmxas;

library ieee;
use ieee.std_logic_1164.all;

architecture mtphxsmee of tpfzzmxas is
  signal xfxbb : integer_vector(2 downto 3);
  signal ibingkhn : boolean;
  signal bh : std_logic_vector(0 to 0);
  signal hhfzthovg : integer_vector(2 downto 3);
  signal bsjynt : boolean;
begin
  mto : entity work.zwpfzefuko
    port map (sgco => bsjynt, mtmx => hhfzthovg);
  joqcrsz : entity work.llrpnveke
    port map (wfuqul => bh);
  lb : entity work.zwpfzefuko
    port map (sgco => ibingkhn, mtmx => xfxbb);
  xxsxlj : entity work.llrpnveke
    port map (wfuqul => bh);
  
  -- Single-driven assignments
  h <= h;
  
  -- Multi-driven assignments
  bh <= bh;
end mtphxsmee;



-- Seed after: 10942585277566580285,5444032936641997477
