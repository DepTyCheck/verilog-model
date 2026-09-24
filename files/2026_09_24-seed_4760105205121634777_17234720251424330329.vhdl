-- Seed: 4760105205121634777,17234720251424330329

entity xzdjndb is
  port (biy : out boolean; titcxqbc : in severity_level);
end xzdjndb;

architecture xajlrqut of xzdjndb is
  
begin
  
end xajlrqut;

entity t is
  port (ncw : inout boolean);
end t;

architecture qq of t is
  signal focj : severity_level;
begin
  nucdsih : entity work.xzdjndb
    port map (biy => ncw, titcxqbc => focj);
  
  -- Single-driven assignments
  focj <= WARNING;
end qq;



-- Seed after: 17134521936366975912,17234720251424330329
