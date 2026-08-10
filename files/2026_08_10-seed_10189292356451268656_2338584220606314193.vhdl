-- Seed: 10189292356451268656,2338584220606314193

entity fpaloyjem is
  port (qcqd : buffer boolean_vector(4 to 2));
end fpaloyjem;

architecture l of fpaloyjem is
  
begin
  -- Single-driven assignments
  qcqd <= qcqd;
end l;



-- Seed after: 11387275118313861640,2338584220606314193
