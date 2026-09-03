-- Seed: 3363361421108246785,11127274767545411571

entity khuak is
  port (raqwrm : buffer time);
end khuak;

architecture n of khuak is
  
begin
  -- Single-driven assignments
  raqwrm <= 10 fs;
end n;



-- Seed after: 2291652467220127088,11127274767545411571
