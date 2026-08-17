-- Seed: 6039599618402301690,13843488114570579517

entity rnef is
  port (ca : inout integer);
end rnef;

architecture f of rnef is
  
begin
  -- Single-driven assignments
  ca <= ca;
end f;



-- Seed after: 1688801850251861172,13843488114570579517
