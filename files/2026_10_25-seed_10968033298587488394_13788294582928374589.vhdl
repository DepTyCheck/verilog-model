-- Seed: 10968033298587488394,13788294582928374589

entity gv is
  port (sr : buffer boolean);
end gv;

architecture idd of gv is
  
begin
  -- Single-driven assignments
  sr <= sr;
end idd;



-- Seed after: 15893204100612566767,13788294582928374589
