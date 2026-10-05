-- Seed: 4479720267349032469,7304262412290825129

entity atthkiqc is
  port (tok : inout boolean_vector(2 to 3); qhvjud : buffer string(4 downto 5));
end atthkiqc;

architecture vhhwxma of atthkiqc is
  
begin
  -- Single-driven assignments
  qhvjud <= (others => ' ');
end vhhwxma;

entity ahi is
  port (vrgrhn : inout severity_level);
end ahi;

architecture qdryjukjaj of ahi is
  signal ol : string(4 downto 5);
  signal fkw : boolean_vector(2 to 3);
  signal ljtxpyybtn : string(4 downto 5);
  signal mcew : boolean_vector(2 to 3);
  signal ktxnlldflm : string(4 downto 5);
  signal rnpao : boolean_vector(2 to 3);
begin
  stzzup : entity work.atthkiqc
    port map (tok => rnpao, qhvjud => ktxnlldflm);
  lwvhnfqueh : entity work.atthkiqc
    port map (tok => mcew, qhvjud => ljtxpyybtn);
  f : entity work.atthkiqc
    port map (tok => fkw, qhvjud => ol);
end qdryjukjaj;

library ieee;
use ieee.std_logic_1164.all;

entity bcbmrstn is
  port (xtltcpunh : in real_vector(2 downto 3); bn : linkage std_logic_vector(4 to 2); kptdlp : buffer real; r : inout real);
end bcbmrstn;

architecture fyvnlowgn of bcbmrstn is
  
begin
  -- Single-driven assignments
  kptdlp <= r;
  r <= 30300.4;
end fyvnlowgn;

library ieee;
use ieee.std_logic_1164.all;

entity iw is
  port (krvg : inout std_logic; qazrl : in time);
end iw;

library ieee;
use ieee.std_logic_1164.all;

architecture papxixo of iw is
  signal t : real;
  signal jwcpsqrslr : real;
  signal udkxacq : real_vector(2 downto 3);
  signal irvilji : string(4 downto 5);
  signal tfinbchl : boolean_vector(2 to 3);
  signal mdgrh : real;
  signal cg : real;
  signal aqxobo : std_logic_vector(4 to 2);
  signal gfacwymju : real_vector(2 downto 3);
  signal tb : severity_level;
begin
  xc : entity work.ahi
    port map (vrgrhn => tb);
  q : entity work.bcbmrstn
    port map (xtltcpunh => gfacwymju, bn => aqxobo, kptdlp => cg, r => mdgrh);
  shfoqeh : entity work.atthkiqc
    port map (tok => tfinbchl, qhvjud => irvilji);
  emyajct : entity work.bcbmrstn
    port map (xtltcpunh => udkxacq, bn => aqxobo, kptdlp => jwcpsqrslr, r => t);
  
  -- Single-driven assignments
  gfacwymju <= (others => 0.0);
end papxixo;



-- Seed after: 18067111406703871938,7304262412290825129
