-- Seed: 8947371566440290330,4177195558088809003

entity dockqi is
  port (l : in severity_level; fym : out bit; kwt : inout real_vector(2 downto 3));
end dockqi;

architecture v of dockqi is
  
begin
  -- Single-driven assignments
  kwt <= (others => 0.0);
  fym <= fym;
end v;



-- Seed after: 14281748958314019056,4177195558088809003
