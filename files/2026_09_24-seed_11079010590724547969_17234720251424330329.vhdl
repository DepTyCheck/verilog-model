-- Seed: 11079010590724547969,17234720251424330329

entity xl is
  port (dvbxno : buffer integer; pb : buffer character);
end xl;

architecture v of xl is
  
begin
  -- Single-driven assignments
  pb <= 'x';
  dvbxno <= 0332;
end v;



-- Seed after: 3754450520172231258,17234720251424330329
