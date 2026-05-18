-- Seed: 4303900720336613791,18238119570016518405

library ieee;
use ieee.std_logic_1164.all;

entity fqpaqjipg is
  port (rmuf : out bit_vector(3 to 4); eildep : linkage real_vector(2 downto 1); blvpxvpy : linkage std_logic);
end fqpaqjipg;



architecture ehgdus of fqpaqjipg is
  
begin
  
end ehgdus;

library ieee;
use ieee.std_logic_1164.all;

entity wxyq is
  port (gmqdk : in std_logic_vector(1 downto 1); tlvzahr : in integer);
end wxyq;

library ieee;
use ieee.std_logic_1164.all;

architecture chdki of wxyq is
  signal lkqd : std_logic;
  signal hzzopjd : real_vector(2 downto 1);
  signal ynvfwnwh : bit_vector(3 to 4);
  signal qhobufaa : std_logic;
  signal hoblrabcn : real_vector(2 downto 1);
  signal mbymrl : bit_vector(3 to 4);
begin
  hkjap : entity work.fqpaqjipg
    port map (rmuf => mbymrl, eildep => hoblrabcn, blvpxvpy => qhobufaa);
  dpez : entity work.fqpaqjipg
    port map (rmuf => ynvfwnwh, eildep => hzzopjd, blvpxvpy => lkqd);
end chdki;



entity ycjuftwm is
  port (odelxgq : buffer integer_vector(2 downto 2));
end ycjuftwm;

library ieee;
use ieee.std_logic_1164.all;

architecture swr of ycjuftwm is
  signal qulrxw : std_logic;
  signal hnzzpawr : real_vector(2 downto 1);
  signal pxaztabp : bit_vector(3 to 4);
  signal tid : integer;
  signal u : std_logic_vector(1 downto 1);
begin
  wekep : entity work.wxyq
    port map (gmqdk => u, tlvzahr => tid);
  jape : entity work.fqpaqjipg
    port map (rmuf => pxaztabp, eildep => hnzzpawr, blvpxvpy => qulrxw);
end swr;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (xmwnlenx : inout real; wlalhf : out std_logic_vector(2 to 3); ffcthx : linkage real);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture ckl of w is
  signal rbjn : integer;
  signal mptvf : std_logic_vector(1 downto 1);
  signal qxczndgn : std_logic;
  signal qaifwfz : bit_vector(3 to 4);
  signal rzazooidv : std_logic;
  signal yrhmnbjor : real_vector(2 downto 1);
  signal nmxxe : bit_vector(3 to 4);
begin
  nlg : entity work.fqpaqjipg
    port map (rmuf => nmxxe, eildep => yrhmnbjor, blvpxvpy => rzazooidv);
  ckifovupf : entity work.fqpaqjipg
    port map (rmuf => qaifwfz, eildep => yrhmnbjor, blvpxvpy => qxczndgn);
  sxel : entity work.wxyq
    port map (gmqdk => mptvf, tlvzahr => rbjn);
end ckl;



-- Seed after: 7648108030374272584,18238119570016518405
