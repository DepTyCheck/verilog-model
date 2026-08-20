-- Seed: 593166267074070851,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity ux is
  port (fnktk : buffer time_vector(1 downto 2); jh : buffer time; izryr : in std_logic; lcafwnfe : out std_logic);
end ux;

architecture mo of ux is
  
begin
  -- Single-driven assignments
  jh <= 2#110.10101# us;
  fnktk <= fnktk;
end mo;

entity uikjufoua is
  port (ucraxrpe : linkage integer; l : linkage integer; gh : linkage bit);
end uikjufoua;

library ieee;
use ieee.std_logic_1164.all;

architecture fnekqv of uikjufoua is
  signal kvzwuemwp : std_logic;
  signal b : time;
  signal sckcdhkpa : time_vector(1 downto 2);
  signal vao : std_logic;
  signal egykpc : std_logic;
  signal c : time;
  signal siemwklsv : time_vector(1 downto 2);
  signal zqh : std_logic;
  signal oackjk : time;
  signal fupy : time_vector(1 downto 2);
  signal jwcuqmziqk : std_logic;
  signal q : time;
  signal kyglyoee : time_vector(1 downto 2);
begin
  rnccye : entity work.ux
    port map (fnktk => kyglyoee, jh => q, izryr => jwcuqmziqk, lcafwnfe => jwcuqmziqk);
  ckmnxxv : entity work.ux
    port map (fnktk => fupy, jh => oackjk, izryr => jwcuqmziqk, lcafwnfe => zqh);
  kxdi : entity work.ux
    port map (fnktk => siemwklsv, jh => c, izryr => egykpc, lcafwnfe => vao);
  hhrb : entity work.ux
    port map (fnktk => sckcdhkpa, jh => b, izryr => zqh, lcafwnfe => kvzwuemwp);
  
  -- Multi-driven assignments
  kvzwuemwp <= jwcuqmziqk;
  zqh <= jwcuqmziqk;
end fnekqv;

library ieee;
use ieee.std_logic_1164.all;

entity gizqogyy is
  port (fslmlo : out std_logic; kfhl : in time; sv : buffer std_logic; okpldwufv : inout time);
end gizqogyy;

library ieee;
use ieee.std_logic_1164.all;

architecture gpamtkxj of gizqogyy is
  signal puhgdpc : std_logic;
  signal esgizxs : std_logic;
  signal tvtwmgvlf : time_vector(1 downto 2);
  signal wlqjrbkk : time;
  signal jrymce : time_vector(1 downto 2);
  signal edkmd : std_logic;
  signal n : std_logic;
  signal btlh : time;
  signal q : time_vector(1 downto 2);
  signal otreevoj : std_logic;
  signal c : time;
  signal wdhmszep : time_vector(1 downto 2);
begin
  fpfaph : entity work.ux
    port map (fnktk => wdhmszep, jh => c, izryr => otreevoj, lcafwnfe => fslmlo);
  lrhtjbyn : entity work.ux
    port map (fnktk => q, jh => btlh, izryr => n, lcafwnfe => edkmd);
  jez : entity work.ux
    port map (fnktk => jrymce, jh => wlqjrbkk, izryr => sv, lcafwnfe => edkmd);
  bvv : entity work.ux
    port map (fnktk => tvtwmgvlf, jh => okpldwufv, izryr => esgizxs, lcafwnfe => puhgdpc);
  
  -- Multi-driven assignments
  sv <= 'H';
  fslmlo <= sv;
  fslmlo <= fslmlo;
  sv <= 'L';
end gpamtkxj;



-- Seed after: 16694236941043079777,499459191852795575
