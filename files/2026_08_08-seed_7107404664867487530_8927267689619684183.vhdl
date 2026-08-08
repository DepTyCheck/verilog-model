-- Seed: 7107404664867487530,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity cqeygx is
  port (kbs : in std_logic_vector(3 to 0); jm : in real; l : in time);
end cqeygx;

architecture kunnfi of cqeygx is
  
begin
  
end kunnfi;

entity zijb is
  port (xw : inout severity_level; lycjaj : inout integer_vector(1 to 2); xkmijqqd : in time; qojpxtrw : buffer real);
end zijb;

library ieee;
use ieee.std_logic_1164.all;

architecture tdjtaryrn of zijb is
  signal viyqzidvia : time;
  signal sxvk : std_logic_vector(3 to 0);
  signal mup : time;
  signal oeuhfwmsg : real;
  signal jl : std_logic_vector(3 to 0);
begin
  koki : entity work.cqeygx
    port map (kbs => jl, jm => oeuhfwmsg, l => mup);
  pjw : entity work.cqeygx
    port map (kbs => sxvk, jm => qojpxtrw, l => mup);
  b : entity work.cqeygx
    port map (kbs => sxvk, jm => qojpxtrw, l => viyqzidvia);
  
  -- Single-driven assignments
  mup <= xkmijqqd;
  xw <= xw;
  
  -- Multi-driven assignments
  jl <= "";
  sxvk <= (others => '0');
  jl <= jl;
end tdjtaryrn;



-- Seed after: 14018128856410733603,8927267689619684183
