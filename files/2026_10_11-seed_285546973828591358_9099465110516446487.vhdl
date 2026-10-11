-- Seed: 285546973828591358,9099465110516446487

entity ymxwmvkglm is
  port (etfb : inout real_vector(2 to 0); es : linkage real);
end ymxwmvkglm;

architecture ozlmernf of ymxwmvkglm is
  
begin
  -- Single-driven assignments
  etfb <= (others => 0.0);
end ozlmernf;

entity gqbk is
  port (hjb : in string(3 to 3));
end gqbk;

architecture achq of gqbk is
  signal ipdznob : real;
  signal robsyp : real_vector(2 to 0);
  signal wdliaw : real;
  signal gdt : real_vector(2 to 0);
begin
  pwiqf : entity work.ymxwmvkglm
    port map (etfb => gdt, es => wdliaw);
  z : entity work.ymxwmvkglm
    port map (etfb => robsyp, es => ipdznob);
end achq;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (bjmfxa : linkage severity_level; at : out bit_vector(2 to 3); btyowmicmb : in std_logic; wgj : buffer std_logic);
end r;

architecture yugxeox of r is
  signal lpeggcczb : real;
  signal ykiamuwnn : real_vector(2 to 0);
  signal ammunjzcra : real;
  signal lgrxgwicm : real_vector(2 to 0);
  signal liqpko : real;
  signal q : real_vector(2 to 0);
  signal zuixuxog : string(3 to 3);
begin
  phkbzyxgi : entity work.gqbk
    port map (hjb => zuixuxog);
  imu : entity work.ymxwmvkglm
    port map (etfb => q, es => liqpko);
  ydqewqe : entity work.ymxwmvkglm
    port map (etfb => lgrxgwicm, es => ammunjzcra);
  avmhhwzntf : entity work.ymxwmvkglm
    port map (etfb => ykiamuwnn, es => lpeggcczb);
  
  -- Single-driven assignments
  zuixuxog <= "c";
  at <= at;
  
  -- Multi-driven assignments
  wgj <= wgj;
end yugxeox;



-- Seed after: 828079423560713653,9099465110516446487
